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

  /* Magic-word easter egg. */
  body.magic-lock {
    overflow: hidden;
  }

  .service-card.magic-trigger {
    cursor: pointer;
  }

  .service-card.magic-trigger:focus-visible {
    outline: 2px solid var(--accent, #59d8ff);
    outline-offset: 4px;
    opacity: 1;
  }

  .magic-hint {
    display: inline-flex;
    align-items: center;
    gap: 7px;
    margin-top: auto;
    padding-top: 22px;
    color: #ffd166;
    font-size: 0.78rem;
    font-weight: 800;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }

  .magic-hint::after {
    content: "↗";
    font-size: 0.95em;
  }

  .magic-overlay[hidden] {
    display: none;
  }

  .magic-overlay {
    position: fixed;
    inset: 0;
    z-index: 5000;
    display: grid;
    place-items: center;
    padding: clamp(12px, 4vw, 40px);
    color: #d8ffe0;
    font-family: "Courier New", ui-monospace, monospace;
  }

  .magic-backdrop {
    position: absolute;
    inset: 0;
    background:
      radial-gradient(circle at center, rgba(0, 86, 67, 0.24), transparent 50%),
      rgba(0, 3, 5, 0.94);
    -webkit-backdrop-filter: blur(9px);
    backdrop-filter: blur(9px);
  }

  .magic-console {
    position: relative;
    width: min(960px, 100%);
    overflow: hidden;
    background: #02090b;
    border: 2px solid #39ff8d;
    border-radius: 4px;
    box-shadow:
      0 0 0 5px #07191a,
      0 0 55px rgba(57, 255, 141, 0.28),
      0 24px 90px rgba(0, 0, 0, 0.72);
    opacity: 0;
    transform: scale(0.94) translateY(18px);
  }

  .magic-overlay.is-open .magic-console {
    animation: magic-boot 240ms steps(3, end) forwards;
  }

  .magic-titlebar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 16px;
    min-height: 42px;
    padding: 7px 8px 7px 14px;
    color: #00180e;
    background: #39ff8d;
    font-size: 0.8rem;
    font-weight: 900;
    letter-spacing: 0.07em;
    text-transform: uppercase;
  }

  .magic-close {
    width: 30px;
    height: 27px;
    padding: 0;
    color: #00180e;
    background: transparent;
    border: 2px solid #00180e;
    font: 900 1rem/1 monospace;
    cursor: pointer;
  }

  .magic-close:hover,
  .magic-close:focus-visible {
    color: #39ff8d;
    background: #00180e;
    outline: none;
  }

  .magic-screen {
    position: relative;
    min-height: min(590px, calc(100vh - 130px));
    display: grid;
    grid-template-columns: minmax(230px, 0.78fr) minmax(0, 1.22fr);
    gap: clamp(24px, 5vw, 58px);
    align-items: center;
    padding: clamp(28px, 6vw, 72px);
    overflow: hidden;
    background:
      radial-gradient(circle at 26% 44%, rgba(57, 255, 141, 0.12), transparent 24rem),
      linear-gradient(135deg, #031211, #010506 68%);
  }

  .magic-screen::before {
    content: "";
    position: absolute;
    inset: 0;
    z-index: 5;
    pointer-events: none;
    background: repeating-linear-gradient(
      0deg,
      rgba(255, 255, 255, 0.025) 0,
      rgba(255, 255, 255, 0.025) 1px,
      transparent 1px,
      transparent 4px
    );
    mix-blend-mode: screen;
  }

  .magic-screen::after {
    content: "";
    position: absolute;
    inset: -35%;
    z-index: 4;
    pointer-events: none;
    background: linear-gradient(100deg, transparent 44%, rgba(120, 255, 182, 0.05) 50%, transparent 56%);
    animation: magic-scan 4.2s linear infinite;
  }

  .magic-avatar-wrap,
  .magic-terminal {
    position: relative;
    z-index: 3;
  }

  .magic-avatar-wrap {
    display: grid;
    justify-items: center;
    gap: 18px;
  }

  .magic-avatar {
    position: relative;
    width: clamp(180px, 25vw, 250px);
    aspect-ratio: 0.92;
    animation: magic-bob 1.5s steps(2, end) infinite;
  }

  .magic-hair {
    position: absolute;
    top: 3%;
    left: 14%;
    width: 72%;
    height: 35%;
    background: #181308;
    border: 7px solid #39ff8d;
    border-bottom: 0;
    border-radius: 48% 48% 25% 25%;
    transform: rotate(-3deg);
  }

  .magic-hair::before,
  .magic-hair::after {
    content: "";
    position: absolute;
    top: -12%;
    width: 38%;
    height: 45%;
    background: #181308;
    border-top: 7px solid #39ff8d;
  }

  .magic-hair::before {
    left: -14%;
    transform: skewX(-25deg) rotate(-18deg);
  }

  .magic-hair::after {
    right: -14%;
    transform: skewX(25deg) rotate(18deg);
  }

  .magic-face {
    position: absolute;
    top: 21%;
    left: 17%;
    width: 66%;
    height: 58%;
    background: #d8ffe0;
    border: 7px solid #39ff8d;
    border-radius: 44% 44% 48% 48%;
    box-shadow: inset 0 -16px 0 rgba(57, 255, 141, 0.2);
  }

  .magic-glasses {
    position: absolute;
    top: 25%;
    left: 10%;
    width: 80%;
    height: 25%;
  }

  .magic-lens {
    position: absolute;
    top: 0;
    width: 38%;
    height: 100%;
    background: #061815;
    border: 5px solid #39ff8d;
    border-radius: 9px;
  }

  .magic-lens:first-child { left: 0; }
  .magic-lens:last-child { right: 0; }

  .magic-lens::after {
    content: "";
    position: absolute;
    top: 34%;
    left: 42%;
    width: 8px;
    height: 8px;
    background: #d8ffe0;
    border-radius: 50%;
    animation: magic-blink 3.3s steps(1, end) infinite;
  }

  .magic-glasses::after {
    content: "";
    position: absolute;
    top: 42%;
    left: 38%;
    width: 24%;
    height: 5px;
    background: #39ff8d;
  }

  .magic-nose {
    position: absolute;
    top: 48%;
    left: 46%;
    width: 10%;
    height: 16%;
    border-right: 4px solid #0a4c31;
    border-bottom: 4px solid #0a4c31;
    transform: skewY(18deg);
  }

  .magic-mouth {
    position: absolute;
    left: 32%;
    bottom: 15%;
    width: 36%;
    height: 7%;
    background: #07100c;
    border: 4px solid #39ff8d;
    border-radius: 0 0 50% 50%;
    transform-origin: center top;
  }

  .magic-avatar.talking .magic-mouth {
    animation: magic-talk 250ms steps(2, end) infinite;
  }

  .magic-shirt {
    position: absolute;
    left: 6%;
    bottom: 0;
    width: 88%;
    height: 32%;
    background: #0a4c31;
    border: 7px solid #39ff8d;
    clip-path: polygon(18% 0, 82% 0, 100% 100%, 0 100%);
  }

  .magic-avatar-caption {
    color: #39ff8d;
    font-size: 0.7rem;
    font-weight: 800;
    letter-spacing: 0.14em;
    text-align: center;
    text-transform: uppercase;
  }

  .magic-command {
    margin: 0 0 18px;
    color: #7effb4;
    font-size: clamp(0.72rem, 1.4vw, 0.9rem);
    letter-spacing: 0.06em;
  }

  .magic-command::before {
    content: "> ";
    color: #ffd166;
  }

  .magic-terminal h2 {
    margin: 0 0 20px;
    color: #ff5364;
    font-size: clamp(2.2rem, 7vw, 5.8rem);
    line-height: 0.88;
    letter-spacing: -0.06em;
    text-shadow: 4px 0 0 rgba(57, 255, 141, 0.2);
    animation: magic-glitch 1.6s steps(2, end) infinite;
  }

  .magic-message {
    margin: 0 0 22px;
    color: #d8ffe0;
    font-size: clamp(1.3rem, 3vw, 2.25rem);
    font-weight: 900;
    line-height: 1.18;
  }

  .magic-message span {
    display: inline;
    background: #0a4c31;
    box-shadow: 9px 0 0 #0a4c31, -9px 0 0 #0a4c31;
  }

  .magic-repeat {
    min-height: 100px;
    color: #7effb4;
    font-size: clamp(0.7rem, 1.3vw, 0.9rem);
    line-height: 1.5;
    opacity: 0.8;
  }

  .magic-repeat-line {
    animation: magic-line-in 180ms steps(3, end) both;
  }

  .magic-status {
    margin-top: 20px;
    padding-top: 12px;
    color: #ffd166;
    border-top: 1px dashed rgba(57, 255, 141, 0.35);
    font-size: 0.72rem;
    letter-spacing: 0.06em;
    text-transform: uppercase;
  }

  .magic-popup {
    position: absolute;
    z-index: 8;
    width: min(260px, 58vw);
    padding: 8px;
    color: #00180e;
    background: #d8ffe0;
    border: 3px solid #39ff8d;
    box-shadow: 8px 8px 0 rgba(255, 83, 100, 0.72);
    font-size: 0.75rem;
    font-weight: 900;
    text-transform: uppercase;
    animation: magic-popup-in 180ms steps(3, end) both;
  }

  .magic-popup strong {
    display: block;
    margin: -8px -8px 7px;
    padding: 4px 7px;
    color: #d8ffe0;
    background: #0a4c31;
  }

  @keyframes magic-boot {
    0% { opacity: 0; transform: scale(0.94) translateY(18px); }
    35% { opacity: 1; transform: scale(1.02) translateY(-4px); }
    100% { opacity: 1; transform: scale(1) translateY(0); }
  }

  @keyframes magic-scan {
    from { transform: translateX(-34%); }
    to { transform: translateX(34%); }
  }

  @keyframes magic-bob {
    0%, 100% { transform: translateY(0) rotate(-1deg); }
    50% { transform: translateY(-5px) rotate(1deg); }
  }

  @keyframes magic-blink {
    0%, 44%, 48%, 100% { transform: scaleY(1); }
    45%, 47% { transform: scaleY(0.08); }
  }

  @keyframes magic-talk {
    0% { height: 7%; transform: scaleY(1); }
    100% { height: 18%; transform: scaleY(1.3); }
  }

  @keyframes magic-glitch {
    0%, 82%, 100% { transform: translate(0); }
    84% { transform: translate(-3px, 1px); }
    86% { transform: translate(4px, -1px); }
    88% { transform: translate(-1px, 0); }
  }

  @keyframes magic-line-in {
    from { opacity: 0; transform: translateX(-10px); }
    to { opacity: 1; transform: translateX(0); }
  }

  @keyframes magic-popup-in {
    from { opacity: 0; transform: scale(0.65) rotate(-3deg); }
    to { opacity: 1; transform: scale(1) rotate(0); }
  }

  @media (max-width: 700px) {
    .magic-screen {
      grid-template-columns: 1fr;
      align-content: center;
      gap: 18px;
      padding: 28px 20px;
    }

    .magic-avatar {
      width: 150px;
    }

    .magic-avatar-caption {
      display: none;
    }

    .magic-repeat {
      min-height: 54px;
    }
  }

  @media (prefers-reduced-motion: reduce) {
    .magic-screen::after,
    .magic-avatar,
    .magic-lens::after,
    .magic-mouth,
    .magic-terminal h2,
    .magic-repeat-line,
    .magic-popup,
    .magic-overlay.is-open .magic-console {
      animation: none !important;
    }

    .magic-overlay.is-open .magic-console {
      opacity: 1;
      transform: none;
    }
  }
</style>`;

const compatibilityBody = String.raw`
<button class="effects-toggle" id="effects-toggle" type="button" aria-pressed="false">Reduce effects</button>

<div class="magic-overlay" id="magic-overlay" hidden>
  <div class="magic-backdrop" data-magic-close></div>
  <section class="magic-console" role="dialog" aria-modal="true" aria-labelledby="magic-title">
    <div class="magic-titlebar">
      <span>Jurassic Systems Security Interface v1.0</span>
      <button class="magic-close" type="button" aria-label="Close access denied screen" data-magic-close>×</button>
    </div>
    <div class="magic-screen" id="magic-screen">
      <div class="magic-avatar-wrap" aria-hidden="true">
        <div class="magic-avatar" id="magic-avatar">
          <div class="magic-hair"></div>
          <div class="magic-face">
            <div class="magic-glasses">
              <span class="magic-lens"></span>
              <span class="magic-lens"></span>
            </div>
            <div class="magic-nose"></div>
            <div class="magic-mouth"></div>
          </div>
          <div class="magic-shirt"></div>
        </div>
        <div class="magic-avatar-caption">Unauthorized access monitor</div>
      </div>
      <div class="magic-terminal">
        <p class="magic-command">C:\\SYSTEM\\SECURITY\\ACCESS.EXE</p>
        <h2 id="magic-title">Access denied</h2>
        <p class="magic-message"><span>Ah ah ah — you didn't say the magic word.</span></p>
        <div class="magic-repeat" id="magic-repeat" aria-live="polite"></div>
        <div class="magic-status">Private infrastructure remains safely unavailable.</div>
      </div>
    </div>
  </section>
</div>

<script>
  (() => {
    const root = document.documentElement;
    const button = document.getElementById('effects-toggle');

    if (button) {
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
    }

    const overlay = document.getElementById('magic-overlay');
    const screen = document.getElementById('magic-screen');
    const avatar = document.getElementById('magic-avatar');
    const repeat = document.getElementById('magic-repeat');
    const closeButton = overlay ? overlay.querySelector('.magic-close') : null;
    const cards = Array.from(document.querySelectorAll('.service-card'));
    const trigger = cards.find((card) => {
      const heading = card.querySelector('h3');
      return heading && heading.textContent.trim().toLowerCase() === 'proxmox';
    });

    if (!overlay || !screen || !avatar || !repeat || !trigger) return;

    trigger.classList.add('magic-trigger');
    trigger.setAttribute('role', 'button');
    trigger.setAttribute('tabindex', '0');
    trigger.setAttribute('aria-haspopup', 'dialog');
    trigger.setAttribute('aria-controls', 'magic-overlay');
    trigger.setAttribute('title', 'Attempt to open the private console');

    if (!trigger.querySelector('.magic-hint')) {
      const hint = document.createElement('span');
      hint.className = 'magic-hint';
      hint.textContent = 'Attempt access';
      trigger.appendChild(hint);
    }

    let repeatTimer = null;
    let popupTimer = null;
    let talkingTimer = null;
    let previousFocus = null;
    let repeatCount = 0;
    let popupCount = 0;

    const reducedMotion = () =>
      root.classList.contains('reduce-effects') ||
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    const clearTimers = () => {
      window.clearInterval(repeatTimer);
      window.clearInterval(popupTimer);
      window.clearInterval(talkingTimer);
      repeatTimer = null;
      popupTimer = null;
      talkingTimer = null;
    };

    const addRepeatLine = () => {
      repeatCount += 1;
      const line = document.createElement('div');
      line.className = 'magic-repeat-line';
      line.textContent = String(repeatCount).padStart(2, '0') + '  AH AH AH — MAGIC WORD REQUIRED';
      repeat.appendChild(line);
      while (repeat.children.length > 5) repeat.firstElementChild.remove();
    };

    const addPopup = () => {
      if (popupCount >= 5) return;
      popupCount += 1;
      const popup = document.createElement('div');
      popup.className = 'magic-popup';
      popup.innerHTML = '<strong>Security alert ' + String(popupCount).padStart(2, '0') + '</strong>Ah ah ah — access denied.';
      const maxLeft = Math.max(8, screen.clientWidth - 280);
      const maxTop = Math.max(8, screen.clientHeight - 120);
      popup.style.left = Math.round(8 + Math.random() * maxLeft) + 'px';
      popup.style.top = Math.round(8 + Math.random() * maxTop) + 'px';
      screen.appendChild(popup);
    };

    const openOverlay = () => {
      previousFocus = document.activeElement;
      repeat.innerHTML = '';
      screen.querySelectorAll('.magic-popup').forEach((popup) => popup.remove());
      repeatCount = 0;
      popupCount = 0;
      overlay.hidden = false;
      document.body.classList.add('magic-lock');
      requestAnimationFrame(() => overlay.classList.add('is-open'));
      if (closeButton) closeButton.focus();

      addRepeatLine();

      if (!reducedMotion()) {
        avatar.classList.add('talking');
        repeatTimer = window.setInterval(addRepeatLine, 720);
        popupTimer = window.setInterval(addPopup, 1050);
        talkingTimer = window.setInterval(() => avatar.classList.toggle('talking'), 1150);
      }
    };

    const closeOverlay = () => {
      if (overlay.hidden) return;
      clearTimers();
      avatar.classList.remove('talking');
      overlay.classList.remove('is-open');
      document.body.classList.remove('magic-lock');
      window.setTimeout(() => {
        overlay.hidden = true;
        screen.querySelectorAll('.magic-popup').forEach((popup) => popup.remove());
        if (previousFocus && typeof previousFocus.focus === 'function') previousFocus.focus();
      }, reducedMotion() ? 0 : 180);
    };

    trigger.addEventListener('click', openOverlay);
    trigger.addEventListener('keydown', (event) => {
      if (event.key === 'Enter' || event.key === ' ') {
        event.preventDefault();
        openOverlay();
      }
    });

    overlay.querySelectorAll('[data-magic-close]').forEach((element) => {
      element.addEventListener('click', closeOverlay);
    });

    document.addEventListener('keydown', (event) => {
      if (event.key === 'Escape' && !overlay.hidden) closeOverlay();
    });
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