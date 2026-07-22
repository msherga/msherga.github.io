import originalWorker from './_worker-original.js';

const credentialLayoutFix = String.raw`
<style>
  .credential-card {
    min-height: 610px !important;
    display: flex !important;
    flex-direction: column !important;
    gap: 26px !important;
    align-items: stretch !important;
  }

  .credential-visual {
    min-height: 340px !important;
    display: grid !important;
    place-items: center !important;
    padding: 20px !important;
    overflow: hidden !important;
    color: #172033 !important;
    background: #ffffff !important;
    border: 1px solid rgba(255, 255, 255, 0.85) !important;
    box-shadow: inset 0 0 0 1px rgba(7, 16, 29, 0.08) !important;
  }

  .credential-visual > div[data-share-badge-id] {
    width: 100% !important;
    min-height: 300px !important;
    display: grid !important;
    place-items: center !important;
  }

  .credential-visual iframe {
    max-width: 100% !important;
  }

  .credential-copy {
    flex: 1 !important;
    justify-content: flex-start !important;
  }

  @media (max-width: 680px) {
    .credential-card {
      min-height: 0 !important;
    }

    .credential-visual {
      min-height: 320px !important;
      padding: 14px !important;
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
          element.setAttribute('data-iframe-width', '300');
          element.setAttribute('data-iframe-height', '300');
        },
      })
      .transform(response);
  },
};
