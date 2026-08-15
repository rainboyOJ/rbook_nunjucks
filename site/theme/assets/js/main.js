(() => {
  const focusableSelector = [
    'a[href]',
    'button:not([disabled])',
    'input:not([disabled])',
    'select:not([disabled])',
    'textarea:not([disabled])',
    '[tabindex]:not([tabindex="-1"])'
  ].join(',');

  function focusableElements(container) {
    return Array.from(container.querySelectorAll(focusableSelector))
      .filter((element) => !element.hidden && element.getClientRects().length > 0);
  }

  function trapFocus(event, container) {
    if (event.key !== 'Tab') return;
    const elements = focusableElements(container);
    if (!elements.length) return;
    const first = elements[0];
    const last = elements[elements.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function initTheme() {
    const controls = Array.from(document.querySelectorAll('.J_darkMode'));
    if (!controls.length) return;

    let savedTheme = 'auto';
    try {
      savedTheme = localStorage.getItem('theme') || 'auto';
    } catch {
      savedTheme = 'auto';
    }
    if (!['auto', 'light', 'dark'].includes(savedTheme)) savedTheme = 'auto';

    const applyTheme = (theme) => {
      document.documentElement.dataset.darkmode = theme;
      controls.forEach((control) => {
        control.checked = control.value === theme;
      });
    };

    applyTheme(savedTheme);
    controls.forEach((control) => {
      control.addEventListener('change', () => {
        if (!control.checked) return;
        applyTheme(control.value);
        try {
          localStorage.setItem('theme', control.value);
        } catch {
          // Theme still applies for this page when storage is unavailable.
        }
      });
    });
  }

  function initMoreMenu() {
    const trigger = document.querySelector('.J_moreToggle');
    const menu = document.querySelector('.reading-more-menu');
    if (!trigger || !menu) return;

    const close = (restoreFocus = false) => {
      menu.hidden = true;
      trigger.setAttribute('aria-expanded', 'false');
      if (restoreFocus) trigger.focus();
    };

    trigger.addEventListener('click', () => {
      const opening = menu.hidden;
      menu.hidden = !opening;
      trigger.setAttribute('aria-expanded', String(opening));
      if (opening) focusableElements(menu)[0]?.focus();
    });

    document.addEventListener('pointerdown', (event) => {
      if (!menu.hidden && !menu.contains(event.target) && !trigger.contains(event.target)) {
        close(false);
      }
    });
    menu.addEventListener('keydown', (event) => {
      if (event.key === 'Escape') {
        event.preventDefault();
        close(true);
      }
    });
  }

  function initBookMenu() {
    const modal = document.querySelector('#modal-menu');
    const trigger = document.querySelector('.J_bookMenu');
    const content = modal?.querySelector('.modal-menu-content');
    if (!modal || !trigger || !content) return;

    let previousFocus = null;
    const close = () => {
      if (modal.hidden) return;
      modal.hidden = true;
      document.body.classList.remove('modal-open');
      trigger.setAttribute('aria-expanded', 'false');
      (previousFocus || trigger).focus();
    };
    const open = () => {
      previousFocus = document.activeElement;
      modal.hidden = false;
      document.body.classList.add('modal-open');
      trigger.setAttribute('aria-expanded', 'true');
      content.focus();
    };

    trigger.addEventListener('click', open);
    modal.querySelector('.J_closeBookMenu')?.addEventListener('click', close);
    modal.querySelector('[data-close-book-menu]')?.addEventListener('click', close);
    modal.addEventListener('keydown', (event) => {
      if (event.key === 'Escape') {
        event.preventDefault();
        close();
        return;
      }
      trapFocus(event, content);
    });
  }

  function initPrint() {
    const printButton = document.querySelector('.J_printArticle');
    if (!printButton || typeof window.print !== 'function') return;

    const updateSourceUrl = () => {
      document.querySelectorAll('.J_printSourceUrl').forEach((sourceUrl) => {
        sourceUrl.textContent = window.location.href;
      });
    };
    updateSourceUrl();
    window.addEventListener('beforeprint', updateSourceUrl);
    printButton.addEventListener('click', () => window.print());
  }

  async function copyText(text) {
    if (navigator.clipboard?.writeText) {
      await navigator.clipboard.writeText(text);
      return;
    }

    const textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.setAttribute('readonly', '');
    textarea.style.position = 'fixed';
    textarea.style.opacity = '0';
    document.body.append(textarea);
    textarea.select();
    const copied = document.execCommand('copy');
    textarea.remove();
    if (!copied) throw new Error('copy failed');
  }

  function languageName(pre) {
    const code = pre.querySelector('code');
    const candidates = [code?.className, pre.className, pre.parentElement?.className]
      .filter(Boolean)
      .join(' ');
    const match = candidates.match(/(?:^|\s)language-([^\s]+)/);
    const name = match?.[1] || 'text';
    return name === 'clike' ? 'cpp' : name;
  }

  function createCopyButton() {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'code-copy-button';
    button.setAttribute('aria-label', '复制代码');
    button.innerHTML = '<i class="fa fa-copy" aria-hidden="true"></i><span>复制</span>';
    return button;
  }

  function bindCopyButton(button, pre) {
    if (button.dataset.copyReady === 'true') return;
    button.dataset.copyReady = 'true';
    button.setAttribute('aria-label', '复制代码');
    button.setAttribute('title', '复制代码');
    if (button.tagName !== 'BUTTON') {
      button.setAttribute('role', 'button');
      button.setAttribute('tabindex', '0');
    }

    const setState = (label, copied) => {
      button.classList.toggle('copied', copied);
      button.setAttribute('aria-label', label);
      const text = button.querySelector('.copy-label') || button.querySelector('span:last-child');
      if (text) text.textContent = label === '已复制' ? '已复制' : '复制';
    };
    const run = async () => {
      try {
        await copyText(pre.querySelector('code')?.textContent || pre.textContent || '');
        setState('已复制', true);
        window.setTimeout(() => setState('复制代码', false), 1500);
      } catch {
        setState('复制失败', false);
        window.setTimeout(() => setState('复制代码', false), 1500);
      }
    };

    button.addEventListener('click', run);
    if (button.tagName !== 'BUTTON') {
      button.addEventListener('keydown', (event) => {
        if (event.key === 'Enter' || event.key === ' ') {
          event.preventDefault();
          run();
        }
      });
    }
  }

  function initCodeBlocks() {
    document.querySelectorAll('.reading-page .markdown-body pre').forEach((pre) => {
      if (pre.matches('.mermaid, .dot, .graphviz') || pre.closest('.graphviz, .viz-contain')) return;

      const numbered = pre.closest('.line-numbers-mode');
      if (numbered) {
        const existingButton = numbered.querySelector('.zeroclipboard-container');
        if (existingButton) bindCopyButton(existingButton, pre);
        return;
      }

      let wrapper = pre.parentElement;
      if (!wrapper || !Array.from(wrapper.classList).some((name) => name.startsWith('language-'))) {
        wrapper = document.createElement('div');
        pre.before(wrapper);
        wrapper.append(pre);
      }
      if (wrapper.classList.contains('code-block')) return;
      wrapper.classList.add('code-block');

      const header = document.createElement('div');
      header.className = 'code-info-header';
      const label = document.createElement('span');
      label.textContent = languageName(pre);
      const button = createCopyButton();
      header.append(label, button);
      wrapper.prepend(header);
      bindCopyButton(button, pre);
    });
  }

  function initTables() {
    document.querySelectorAll('.reading-page .markdown-body table').forEach((table) => {
      if (table.parentElement?.classList.contains('table-scroll')) return;
      const wrapper = document.createElement('div');
      wrapper.className = 'table-scroll';
      wrapper.setAttribute('role', 'region');
      wrapper.setAttribute('aria-label', '可横向滚动的表格');
      wrapper.tabIndex = 0;
      table.before(wrapper);
      wrapper.append(table);
    });
  }

  function initHeadingAnchors() {
    document.querySelectorAll('.reading-page .markdown-body :is(h2, h3)[id]').forEach((heading) => {
      if (heading.querySelector('.heading-anchor')) return;
      const anchor = document.createElement('a');
      anchor.className = 'heading-anchor';
      anchor.href = `#${heading.id}`;
      anchor.textContent = '#';
      anchor.setAttribute('aria-label', `链接到“${heading.textContent.trim()}”`);
      heading.prepend(anchor);
    });
  }

  function initImageLightbox() {
    const images = Array.from(document.querySelectorAll('.reading-page .markdown-body img:not(.emoji)'))
      .filter((image) => !image.closest('a'));
    if (!images.length || typeof HTMLDialogElement === 'undefined') return;

    const dialog = document.createElement('dialog');
    dialog.className = 'article-lightbox';
    dialog.innerHTML = '<button class="article-lightbox-close" type="button" aria-label="关闭大图"><i class="fa fa-times" aria-hidden="true"></i></button><img alt="">';
    document.body.append(dialog);
    const preview = dialog.querySelector('img');
    const closeButton = dialog.querySelector('button');
    let sourceImage = null;

    const close = () => dialog.open && dialog.close();
    const open = (image) => {
      sourceImage = image;
      preview.src = image.currentSrc || image.src;
      preview.alt = image.alt || '';
      dialog.showModal();
      closeButton.focus();
    };
    closeButton.addEventListener('click', close);
    dialog.addEventListener('click', (event) => {
      if (event.target === dialog) close();
    });
    dialog.addEventListener('close', () => sourceImage?.focus());

    images.forEach((image) => {
      image.dataset.lightboxReady = 'true';
      image.tabIndex = 0;
      image.setAttribute('role', 'button');
      image.setAttribute('aria-label', `${image.alt || '文章图片'}，查看大图`);
      image.addEventListener('click', () => open(image));
      image.addEventListener('keydown', (event) => {
        if (event.key === 'Enter' || event.key === ' ') {
          event.preventDefault();
          open(image);
        }
      });
    });
  }

  function initToc() {
    const toc = document.querySelector('.reading-page .table-of-contents');
    const toggles = Array.from(document.querySelectorAll('.J_tocToggle'));
    if (!toc) {
      toggles.forEach((toggle) => toggle.hidden = true);
      return;
    }

    toc.id = 'article-toc';
    toc.tabIndex = -1;
    toc.setAttribute('aria-label', '本页目录');

    const header = document.createElement('div');
    header.className = 'article-toc-header';
    header.innerHTML = '<strong class="article-toc-title">本页目录</strong><button class="article-toc-close" type="button" aria-label="关闭本页目录"><i class="fa fa-times" aria-hidden="true"></i></button>';
    toc.prepend(header);

    const backdrop = document.createElement('button');
    backdrop.type = 'button';
    backdrop.className = 'toc-backdrop';
    backdrop.hidden = true;
    backdrop.setAttribute('aria-label', '关闭本页目录');
    document.body.append(backdrop);

    let restoreFocus = null;
    const isDrawer = () => window.matchMedia('(max-width: 1379px)').matches;
    const siteFooter = document.querySelector('.site-footer');
    let tocLayoutScheduled = false;
    const updateTocFooterLimit = () => {
      tocLayoutScheduled = false;
      if (isDrawer() || !siteFooter) {
        toc.style.removeProperty('--toc-footer-limit');
        return;
      }
      const tocTop = toc.getBoundingClientRect().top;
      const footerTop = siteFooter.getBoundingClientRect().top;
      const availableHeight = Math.max(0, footerTop - tocTop - 16);
      toc.style.setProperty('--toc-footer-limit', `${availableHeight}px`);
    };
    const scheduleTocLayout = () => {
      if (tocLayoutScheduled) return;
      tocLayoutScheduled = true;
      requestAnimationFrame(updateTocFooterLimit);
    };
    const setExpanded = (expanded) => {
      toggles.forEach((toggle) => toggle.setAttribute('aria-expanded', String(expanded)));
    };
    const close = (restore = true) => {
      document.body.classList.remove('toc-open');
      backdrop.hidden = true;
      setExpanded(!isDrawer());
      if (restore) restoreFocus?.focus();
    };
    const open = (toggle) => {
      if (!isDrawer()) {
        setExpanded(true);
        toc.focus({ preventScroll: true });
        return;
      }
      restoreFocus = toggle;
      document.body.classList.add('toc-open');
      backdrop.hidden = false;
      setExpanded(true);
      header.querySelector('button').focus();
    };

    toggles.forEach((toggle) => toggle.addEventListener('click', () => open(toggle)));
    header.querySelector('button').addEventListener('click', () => close());
    backdrop.addEventListener('click', () => close());
    toc.addEventListener('keydown', (event) => {
      if (!document.body.classList.contains('toc-open')) return;
      if (event.key === 'Escape') {
        event.preventDefault();
        close();
        return;
      }
      trapFocus(event, toc);
    });
    toc.addEventListener('click', (event) => {
      if (event.target.closest('a') && isDrawer()) close(false);
    });
    const syncTocMode = () => {
      if (!isDrawer()) {
        close(false);
        setExpanded(true);
      } else if (!document.body.classList.contains('toc-open')) {
        setExpanded(false);
      }
      scheduleTocLayout();
    };
    window.addEventListener('resize', syncTocMode);
    window.addEventListener('scroll', scheduleTocLayout, { passive: true });
    window.addEventListener('load', scheduleTocLayout);
    syncTocMode();

    const headings = Array.from(document.querySelectorAll('.reading-page .markdown-body :is(h2, h3)[id]'));
    const links = Array.from(toc.querySelectorAll('a[href^="#"]'));
    const keepLinkVisible = (link) => {
      if (isDrawer() && !document.body.classList.contains('toc-open')) return;
      const tocRect = toc.getBoundingClientRect();
      const linkRect = link.getBoundingClientRect();
      const headerInset = isDrawer() ? header.getBoundingClientRect().height : 0;
      const visibleTop = tocRect.top + headerInset + 8;
      const visibleBottom = tocRect.bottom - 8;
      if (linkRect.top < visibleTop) {
        toc.scrollTop -= visibleTop - linkRect.top;
      } else if (linkRect.bottom > visibleBottom) {
        toc.scrollTop += linkRect.bottom - visibleBottom;
      }
    };
    let scheduled = false;
    const updateActive = () => {
      scheduled = false;
      if (!headings.length || !links.length) return;
      let active = headings[0];
      for (const heading of headings) {
        if (heading.getBoundingClientRect().top <= 140) active = heading;
        else break;
      }
      links.forEach((link) => {
        const current = link.getAttribute('href') === `#${active.id}`;
        link.classList.toggle('active', current);
        if (current) link.setAttribute('aria-current', 'location');
        else link.removeAttribute('aria-current');
      });
      const activeLink = links.find((link) => link.getAttribute('aria-current') === 'location');
      if (activeLink) keepLinkVisible(activeLink);
    };
    const scheduleUpdate = () => {
      if (scheduled) return;
      scheduled = true;
      requestAnimationFrame(updateActive);
    };
    window.addEventListener('scroll', scheduleUpdate, { passive: true });
    updateActive();
  }

  function normalizeArticlePath(value) {
    try {
      const url = new URL(value, window.location.origin);
      let pathname = decodeURIComponent(url.pathname).replace(/\/index\.html$/, '');
      pathname = pathname.replace(/\.html$/, '').replace(/\/$/, '');
      return pathname || '/';
    } catch {
      return '';
    }
  }

  function initArticleNavigation() {
    const menu = document.querySelector('#modal-menu .chapter-list');
    if (!menu || !document.querySelector('.reading-page')) return;
    const currentPath = normalizeArticlePath(window.location.href);
    const links = Array.from(menu.querySelectorAll('a[href]')).filter((link) => {
      const href = link.getAttribute('href') || '';
      return !href.startsWith('javascript:') && href !== '#';
    });
    const index = links.findIndex((link) => normalizeArticlePath(link.href) === currentPath);
    if (index < 0) return;

    links[index].setAttribute('aria-current', 'page');
    const setLink = (selector, source) => {
      const target = document.querySelector(selector);
      if (!target || !source) return;
      target.href = source.href;
      target.querySelector('strong').textContent = source.textContent.trim();
      target.hidden = false;
    };
    setLink('.J_previousArticle', links[index - 1]);
    setLink('.J_nextArticle', links[index + 1]);
  }

  async function hydratePrerequisites() {
    const links = Array.from(document.querySelectorAll('.J_prerequisiteLink'));
    if (!links.length) return;
    try {
      const response = await fetch('/api/catalog?includeHidden=true', {
        headers: { Accept: 'application/json' }
      });
      if (!response.ok) return;
      const payload = await response.json();
      const pages = new Map((payload.items || []).map((page) => [page.id, page]));
      links.forEach((link) => {
        const page = pages.get(link.dataset.prerequisiteId);
        if (!page) return;
        link.textContent = page.title || page.id;
        if (page.url) link.href = page.url;
      });
    } catch {
      // The relation view remains a useful fallback when the API is unavailable.
    }
  }

  function initMobileToolbar() {
    const toolbar = document.querySelector('.J_readingToolbar');
    if (!toolbar || !document.body.classList.contains('printable-article')) return;
    let previousY = window.scrollY;
    let scheduled = false;
    const update = () => {
      scheduled = false;
      if (!window.matchMedia('(max-width: 767px)').matches) {
        toolbar.classList.remove('panel-hidden');
        previousY = window.scrollY;
        return;
      }
      const currentY = window.scrollY;
      const delta = currentY - previousY;
      if (delta > 10 && currentY > 110 && !document.body.classList.contains('toc-open')) {
        toolbar.classList.add('panel-hidden');
      } else if (delta < -8 || currentY < 40) {
        toolbar.classList.remove('panel-hidden');
      }
      previousY = currentY;
    };
    window.addEventListener('scroll', () => {
      if (scheduled) return;
      scheduled = true;
      requestAnimationFrame(update);
    }, { passive: true });
  }

  document.addEventListener('DOMContentLoaded', () => {
    initTheme();
    initMoreMenu();
    initBookMenu();
    initPrint();
    initCodeBlocks();
    initTables();
    initHeadingAnchors();
    initImageLightbox();
    initToc();
    initArticleNavigation();
    hydratePrerequisites();
    initMobileToolbar();
  });
})();
