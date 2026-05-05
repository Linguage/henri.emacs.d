document.addEventListener('DOMContentLoaded', function () {
  var content = document.getElementById('content');
  if (!content) return;

  var root = document.documentElement;
  var body = document.body;
  var currentTheme = null;
  var themeToggleBtn = null;
  var tocModalBackdrop = null;
  var previouslyFocused = null;

  function cleanupLegacyThemes() {
    Array.prototype.slice.call(document.querySelectorAll('#hb-topbar, .hj-site-header, .hj-activity-launcher, .hj-modal-backdrop')).forEach(function (el) {
      if (el.parentNode) el.parentNode.removeChild(el);
    });

    Array.prototype.slice.call(content.querySelectorAll('.hb-fold-toggle')).forEach(function (el) {
      if (el.parentNode) el.parentNode.removeChild(el);
    });

    Array.prototype.slice.call(content.querySelectorAll('.hb-collapsible-heading, .hb-heading-collapsed')).forEach(function (el) {
      el.classList.remove('hb-collapsible-heading');
      el.classList.remove('hb-heading-collapsed');
    });

    Array.prototype.slice.call(content.querySelectorAll('.hb-section-body')).forEach(function (wrapper) {
      wrapper.classList.remove('hb-collapsed');
      wrapper.removeAttribute('data-collapsed');
      while (wrapper.firstChild) {
        wrapper.parentNode.insertBefore(wrapper.firstChild, wrapper);
      }
      wrapper.parentNode.removeChild(wrapper);
    });

    var legacyHeader = content.querySelector(':scope > header');
    if (legacyHeader) legacyHeader.parentNode.removeChild(legacyHeader);
  }

  function applyTheme(theme) {
    if (theme !== 'light' && theme !== 'dark') return;
    currentTheme = theme;
    root.setAttribute('data-theme', theme);
    root.classList.toggle('light', theme === 'light');
    root.classList.toggle('dark', theme === 'dark');

    try {
      window.localStorage.setItem('hn-theme', theme);
    } catch (e) {}

    if (themeToggleBtn) {
      themeToggleBtn.textContent = theme === 'dark' ? 'Light Mode' : 'Night Mode';
      themeToggleBtn.setAttribute('aria-label', theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode');
    }
  }

  function initTheme() {
    var stored = null;
    try {
      stored = window.localStorage.getItem('hn-theme');
    } catch (e) {}

    var initial = stored;
    if (initial !== 'light' && initial !== 'dark') {
      initial = root.getAttribute('data-theme');
    }
    if (initial !== 'light' && initial !== 'dark') {
      initial = 'light';
    }
    applyTheme(initial);
  }

  function cleanText(text) {
    return (text || '').replace(/\s+/g, ' ').trim();
  }

  function isMetadataLine(line) {
    return /^:(?:PROPERTIES|END|ID|CREATED|UPDATED|ROAM_[A-Z0-9_]+|CUSTOM_ID|FILETAGS|LAST_MODIFIED|REFS?):\s*/i.test(line);
  }

  function removeExportedMetadataParagraphs() {
    var stopTags = { H2: true, H3: true, H4: true, H5: true, H6: true };
    var candidates = Array.prototype.slice.call(content.children);

    candidates.some(function (el) {
      if (stopTags[el.tagName]) return true;
      if (el.id === 'table-of-contents') return false;
      if (el.classList.contains('outline-2')) return true;
      if (el.tagName !== 'P') return false;

      var lines = (el.textContent || '').split(/\n+/).map(function (line) {
        return line.trim();
      }).filter(Boolean);

      if (lines.length && lines.every(isMetadataLine)) {
        el.parentNode.removeChild(el);
      }
      return false;
    });
  }

  function documentTitle() {
    var titleHeading = content.querySelector('h1.title');
    return cleanText(titleHeading ? titleHeading.textContent : document.title) || 'Untitled';
  }

  function movePostambleMeta() {
    var postamble = document.getElementById('postamble');
    var meta = [];
    if (!postamble) return meta;

    ['author', 'date'].forEach(function (className) {
      var el = postamble.querySelector('.' + className);
      if (!el) return;
      var text = cleanText(el.textContent.replace(/^(Author|Created):\s*/i, ''));
      if (text) meta.push(text);
    });

    postamble.parentNode.removeChild(postamble);
    return meta;
  }

  function sourceToc() {
    var toc = document.getElementById('table-of-contents');
    if (!toc) return null;
    var inner = toc.querySelector('#text-table-of-contents') || toc;
    return inner.querySelector('a') ? inner : null;
  }

  function buildHeader(hasToc) {
    var metaValues = movePostambleMeta();

    var header = document.createElement('header');
    header.className = 'hn-site-header';

    var inner = document.createElement('div');
    inner.className = 'hn-site-header-inner';
    header.appendChild(inner);

    var titleBlock = document.createElement('div');
    titleBlock.className = 'hn-title-block';
    inner.appendChild(titleBlock);

    var kicker = document.createElement('p');
    kicker.className = 'hn-kicker';
    kicker.textContent = 'Org Notes';
    titleBlock.appendChild(kicker);

    var titleRow = document.createElement('div');
    titleRow.className = 'hn-title-row';
    titleBlock.appendChild(titleRow);

    var h1 = document.createElement('h1');
    h1.className = 'hn-page-title';
    h1.textContent = documentTitle();
    titleRow.appendChild(h1);

    if (metaValues.length) {
      var meta = document.createElement('div');
      meta.className = 'hn-meta';
      metaValues.forEach(function (value) {
        var item = document.createElement('span');
        item.className = 'hn-meta-item';
        item.textContent = value;
        meta.appendChild(item);
      });
      titleBlock.appendChild(meta);
    }

    var actions = document.createElement('div');
    actions.className = 'hn-header-actions';
    inner.appendChild(actions);

    if (hasToc) {
      var tocBtn = document.createElement('button');
      tocBtn.className = 'hn-button';
      tocBtn.type = 'button';
      tocBtn.textContent = 'Contents';
      tocBtn.addEventListener('click', openTocModal);
      actions.appendChild(tocBtn);
    }

    var themeBtn = document.createElement('button');
    themeBtn.id = 'hn-theme-toggle';
    themeBtn.className = 'hn-button';
    themeBtn.type = 'button';
    themeBtn.addEventListener('click', function () {
      applyTheme(currentTheme === 'dark' ? 'light' : 'dark');
    });
    themeToggleBtn = themeBtn;
    actions.appendChild(themeBtn);

    body.insertBefore(header, content);
    applyTheme(currentTheme || 'light');
  }

  function openTocModal() {
    if (!tocModalBackdrop) return;
    previouslyFocused = document.activeElement;
    tocModalBackdrop.classList.add('hn-open');
    body.classList.add('hn-modal-open');
    tocModalBackdrop.setAttribute('aria-hidden', 'false');
    var close = tocModalBackdrop.querySelector('#hn-toc-close');
    if (close) close.focus();
  }

  function closeTocModal() {
    if (!tocModalBackdrop) return;
    tocModalBackdrop.classList.remove('hn-open');
    body.classList.remove('hn-modal-open');
    tocModalBackdrop.setAttribute('aria-hidden', 'true');
    if (previouslyFocused && previouslyFocused.focus) previouslyFocused.focus();
  }

  function buildTocModal(tocSource) {
    if (!tocSource) return;

    var launcher = document.createElement('div');
    launcher.className = 'hn-toc-launcher';
    var openBtn = document.createElement('button');
    openBtn.id = 'hn-toc-open';
    openBtn.className = 'hn-button';
    openBtn.type = 'button';
    openBtn.textContent = 'Contents';
    launcher.appendChild(openBtn);
    body.appendChild(launcher);

    tocModalBackdrop = document.createElement('div');
    tocModalBackdrop.className = 'hn-modal-backdrop';
    tocModalBackdrop.setAttribute('aria-hidden', 'true');
    tocModalBackdrop.setAttribute('role', 'dialog');
    tocModalBackdrop.setAttribute('aria-modal', 'true');
    tocModalBackdrop.setAttribute('aria-labelledby', 'hn-toc-title');

    var modal = document.createElement('div');
    modal.className = 'hn-modal';
    tocModalBackdrop.appendChild(modal);

    var header = document.createElement('div');
    header.className = 'hn-modal-header';
    modal.appendChild(header);

    var title = document.createElement('h2');
    title.id = 'hn-toc-title';
    title.className = 'hn-modal-title';
    title.textContent = 'Contents';
    header.appendChild(title);

    var closeBtn = document.createElement('button');
    closeBtn.id = 'hn-toc-close';
    closeBtn.className = 'hn-button hn-icon-button';
    closeBtn.type = 'button';
    closeBtn.setAttribute('aria-label', 'Close contents');
    closeBtn.textContent = 'x';
    header.appendChild(closeBtn);

    var tocWrap = document.createElement('div');
    tocWrap.className = 'hn-toc';
    var clonedToc = tocSource.cloneNode(true);
    clonedToc.removeAttribute('id');
    Array.prototype.slice.call(clonedToc.querySelectorAll('[id]')).forEach(function (el) {
      el.removeAttribute('id');
    });
    Array.prototype.slice.call(clonedToc.querySelectorAll('a[href^="#"]')).forEach(function (link) {
      link.addEventListener('click', closeTocModal);
    });
    tocWrap.appendChild(clonedToc);
    modal.appendChild(tocWrap);

    body.appendChild(tocModalBackdrop);

    openBtn.addEventListener('click', openTocModal);
    closeBtn.addEventListener('click', closeTocModal);
    tocModalBackdrop.addEventListener('click', function (ev) {
      if (ev.target === tocModalBackdrop) closeTocModal();
    });
    document.addEventListener('keydown', function (ev) {
      if (ev.key === 'Escape' && tocModalBackdrop.classList.contains('hn-open')) closeTocModal();
    });
  }

  cleanupLegacyThemes();
  initTheme();
  removeExportedMetadataParagraphs();
  var toc = sourceToc();
  buildHeader(Boolean(toc));
  buildTocModal(toc);
  window.setTimeout(cleanupLegacyThemes, 0);
  window.setTimeout(cleanupLegacyThemes, 150);
});
