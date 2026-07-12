const compatibilityHead = String.raw`
<script>
  document.documentElement.classList.add('js');
  try {
    if (localStorage.getItem('reduced-effects') === 'true') {
      document.documentElement.classList.add('reduce-effects');
    }
  } catch (_) {}
</script>
<style>
  /* Safe fallbacks when JavaScript is unavailable. */
  html:not(.js) .reveal {
    opacity: 1 !important;
    transform: none !important;
  }

  /* Browsers without backdrop-filter get a nearly opaque header. */
  @supports not ((backdrop-filter: blur(1px)) or (-webkit-backdrop-filter: blur(1px))) {
    .site-header {
      background: rgba(7, 16, 29, 0.98) !important;
    }
  }

  /* Reduce costly decoration on small screens. */
  @media (max-width: 680px) {
    body::before {
      display: none;
    }

    .site-header {
      -webkit-backdrop-filter: blur(8px);
      backdrop-filter: blur(8px);
    }
  }

  /* Respect operating-system accessibility preferences. */
  @media (prefers-reduced-motion: reduce) {
    html {
      scroll-behavior: auto !important;
    }

    *, *::before, *::after {
      animation-duration: 0.01ms !important;
      animation-iteration-count: 1 !important;
      scroll-behavior: auto !important;
      transition-duration: 0.01ms !important;
    }

    body::before,
    .terminal-card::after {
      display: none !important;
    }

    .reveal {
      opacity: 1 !important;
      transform: none !important;
    }
  }

  @media (prefers-reduced-transparency: reduce) {
    .site-header {
      background: #07101d !important;
      -webkit-backdrop-filter: none !important;
      backdrop-filter: none !important;
    }

    .panel,
    .project-card,
    .study-card,
    .service-card {
      background: #10223a !important;
    }
  }

  /* Manual fallback for software-rendered or older systems. */
  html.reduce-effects {
    scroll-behavior: auto !important;
  }

  html.reduce-effects body::before,
  html.reduce-effects .terminal-card::after {
    display: none !important;
  }

  html.reduce-effects .site-header {
    background: #07101d !important;
    -webkit-backdrop-filter: none !important;
    backdrop-filter: none !important;
  }

  html.reduce-effects .reveal {
    opacity: 1 !important;
    transform: none !important;
    transition: none !important;
  }

  html.reduce-effects *,
  html.reduce-effects *::before,
  html.reduce-effects *::after {
    animation: none !important;
  }

  .effects-toggle {
    position: fixed;
    right: 14px;
    bottom: 14px;
    z-index: 999;
    padding: 9px 12px;
    color: #dce7f3;
    background: rgba(7, 16, 29, 0.94);
    border: 1px solid rgba(151, 184, 219, 0.24);
    border-radius: 10px;
    font: 650 0.78rem/1 system-ui, sans-serif;
    cursor: pointer;
  }

  .effects-toggle:hover,
  .effects-toggle:focus-visible {
    color: #fff;
    border-color: rgba(89, 216, 255, 0.55);
    outline: none;
  }
</style>`;

const compatibilityBody = String.raw`
<button class="effects-toggle" id="effects-toggle" type="button" aria-pressed="false">Reduce effects</button>
<script>
  (() => {
    const root = document.documentElement;
    const button = document.getElementById('effects-toggle');
    if (!button) return;

    const update = () => {
      const reduced = root.classList.contains('reduce-effects');
      button.textContent = reduced ? 'Enable effects' : 'Reduce effects';
      button.setAttribute('aria-pressed', String(reduced));
    };

    button.addEventListener('click', () => {
      const reduced = root.classList.toggle('reduce-effects');
      try {
        localStorage.setItem('reduced-effects', String(reduced));
      } catch (_) {}
      update();
    });

    update();
  })();
</script>`;

export default {
  async fetch(request, env) {
    const response = await env.ASSETS.fetch(request);
    const url = new URL(request.url);
    const contentType = response.headers.get('content-type') || '';

    if (!contentType.includes('text/html') || !['/', '/index.html'].includes(url.pathname)) {
      return response;
    }

    return new HTMLRewriter()
      .on('head', {
        element(element) {
          element.append(compatibilityHead, { html: true });
        },
      })
      .on('body', {
        element(element) {
          element.append(compatibilityBody, { html: true });
        },
      })
      .transform(response);
  },
};
