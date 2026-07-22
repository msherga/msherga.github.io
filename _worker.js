import originalWorker from './_worker-original.js';

const credentialLayoutFix = String.raw`
<style>
  .credential-card {
    min-height: 340px !important;
    display: grid !important;
    grid-template-columns: minmax(220px, 0.78fr) minmax(0, 1.22fr) !important;
    gap: 24px !important;
    align-items: center !important;
    padding: 28px !important;
  }

  .credential-visual {
    min-height: 290px !important;
    display: grid !important;
    place-items: center !important;
    padding: 10px !important;
    overflow: hidden !important;
    color: #172033 !important;
    background: #ffffff !important;
    border: 1px solid rgba(255, 255, 255, 0.85) !important;
    box-shadow: inset 0 0 0 1px rgba(7, 16, 29, 0.08) !important;
  }

  .credential-visual > div[data-share-badge-id] {
    position: relative !important;
    width: 220px !important;
    height: 220px !important;
    min-height: 0 !important;
    overflow: hidden !important;
  }

  .credential-visual > div[data-share-badge-id] iframe {
    position: absolute !important;
    top: -10px !important;
    left: 50% !important;
    width: 210px !important;
    height: 270px !important;
    max-width: none !important;
    border: 0 !important;
    transform: translateX(-50%) scale(1.6) !important;
    transform-origin: top center !important;
  }

  .credential-copy {
    flex: initial !important;
    justify-content: center !important;
  }

  @media (max-width: 680px) {
    .credential-card {
      min-height: 0 !important;
      grid-template-columns: 1fr !important;
      gap: 22px !important;
      padding: 22px !important;
    }

    .credential-visual {
      min-height: 260px !important;
      padding: 10px !important;
    }
  }
</style>`;

export default {
  async fetch(request, env, ctx) {
    const response = await originalWorker.fetch(request, env, ctx);
    const url = new URL(request.url);
    const contentType = response.headers.get('content-type') || '';

    if (!contentType.includes('text/html') || !['/', '/index.html'].includes(url.pathname)) {
      return response;
    }

    return new HTMLRewriter()
      .on('head', {
        element(element) {
          element.append(credentialLayoutFix, { html: true });
        },
      })
      .on('[data-share-badge-id]', {
        element(element) {
          element.setAttribute('data-iframe-width', '210');
          element.setAttribute('data-iframe-height', '270');
        },
      })
      .transform(response);
  },
};
