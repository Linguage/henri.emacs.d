document.addEventListener('DOMContentLoaded', function () {
  var content = document.getElementById('content');
  if (!content) return;

  var root = document.documentElement;
  var themeToggleBtn = null;
  var currentTheme = null;

  function applyTheme(theme) {
    if (theme !== 'light' && theme !== 'dark') return;
    currentTheme = theme;

    if (theme === 'light') {
      root.setAttribute('data-theme', 'light');
      root.classList.add('light');
      root.classList.remove('dark');
    } else {
      root.setAttribute('data-theme', 'dark');
      root.classList.add('dark');
      root.classList.remove('light');
    }

    try {
      window.localStorage.setItem('hb-theme', theme);
    } catch (e) {}

    if (themeToggleBtn) {
      themeToggleBtn.textContent = theme === 'dark' ? '日间模式' : '夜间模式';
    }
  }

  (function initTheme() {
    var stored = null;
    try {
      stored = window.localStorage.getItem('hb-theme');
    } catch (e) {}

    var initial = stored;
    if (initial !== 'light' && initial !== 'dark') {
      initial = root.getAttribute('data-theme');
    }
    if (initial !== 'light' && initial !== 'dark') {
      initial = (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches)
        ? 'dark'
        : 'light';
    }
    applyTheme(initial);
  })();

  // 只对正文区域的 h2/h3/h4 启用折叠（对应 Org 的前三级标题），排除文档主标题和目录里的标题
  var headings = Array.prototype.slice.call(
    content.querySelectorAll('h1, h2, h3, h4')
  ).filter(function (h) {
    // 文档主标题不折叠
    if (h.tagName === 'H1' && h.classList.contains('title')) return false;
    // 目录区域中的标题不折叠
    if (h.closest('#table-of-contents')) return false;
    return true;
  });

  if (!headings.length) return;

  var sections = [];

  function collectSection(heading) {
    var level = parseInt(heading.tagName.substr(1), 10);
    var bodyNodes = [];
    var el = heading.nextElementSibling;
    while (el) {
      if (/^H[1-6]$/.test(el.tagName)) {
        var nextLevel = parseInt(el.tagName.substr(1), 10);
        if (nextLevel <= level) break;
      }
      bodyNodes.push(el);
      el = el.nextElementSibling;
    }
    if (!bodyNodes.length) return null;

    var wrapper = document.createElement('div');
    wrapper.className = 'hb-section-body';
    heading.parentNode.insertBefore(wrapper, bodyNodes[0]);
    bodyNodes.forEach(function (n) { wrapper.appendChild(n); });
    return wrapper;
  }

  headings.forEach(function (h) {
    var level = parseInt(h.tagName.substr(1), 10);
    // 只处理 h2/h3/h4
    if (level < 2 || level > 4) return;

    var body = collectSection(h);
    if (!body) return;

    h.classList.add('hb-collapsible-heading');

    var toggle = document.createElement('span');
    toggle.className = 'hb-fold-toggle';
    toggle.textContent = ' ▾';
    h.appendChild(toggle);

    function setCollapsed(collapsed) {
      if (collapsed) {
        body.classList.add('hb-collapsed');
        h.classList.add('hb-heading-collapsed');
        toggle.textContent = ' ▸';
      } else {
        body.classList.remove('hb-collapsed');
        h.classList.remove('hb-heading-collapsed');
        toggle.textContent = ' ▾';
      }
      body.setAttribute('data-collapsed', collapsed ? '1' : '0');
    }

    setCollapsed(false);

    h.addEventListener('click', function (ev) {
      // 避免点击标题里的链接时误触折叠
      if (ev.target.closest('a')) return;
      var collapsed = body.getAttribute('data-collapsed') === '1';
      setCollapsed(!collapsed);
    });

    sections.push({ heading: h, body: body, setCollapsed: setCollapsed });
  });

  if (!sections.length) return;

  // 全局折叠/展开控制按钮
  var controls = document.createElement('div');
  controls.id = 'hb-fold-controls';
  var btn = document.createElement('button');
  btn.id = 'hb-fold-toggle-all';
  btn.type = 'button';

  var allCollapsed = false;

  function updateButtonLabel() {
    btn.textContent = allCollapsed ? '全部展开' : '全部折叠';
  }

  updateButtonLabel();

  btn.addEventListener('click', function () {
    allCollapsed = !allCollapsed;
    sections.forEach(function (s) { s.setCollapsed(allCollapsed); });
    updateButtonLabel();
  });

  controls.appendChild(btn);

  // 创建顶栏：标题 + 作者日期 + 当前小节 + 全局折叠按钮
  var body = document.body;
  var topbar = document.createElement('div');
  topbar.id = 'hb-topbar';
  var inner = document.createElement('div');
  inner.id = 'hb-topbar-inner';
  topbar.appendChild(inner);

  var left = document.createElement('div');
  left.id = 'hb-topbar-left';
  var right = document.createElement('div');
  right.id = 'hb-topbar-right';
  inner.appendChild(left);
  inner.appendChild(right);

  var header = content.querySelector('header');
  var titleHeading = header ? header.querySelector('h1.title') : content.querySelector('h1.title');
  var docTitleText = titleHeading ? titleHeading.textContent.trim() : (document.title || '');

  var titleBox = document.createElement('div');
  titleBox.id = 'hb-topbar-title';
  titleBox.textContent = docTitleText;
  left.appendChild(titleBox);

  var postamble = document.getElementById('postamble');
  if (postamble) {
    var author = postamble.querySelector('.author');
    var date = postamble.querySelector('.date');

    if (author || date) {
      var meta = document.createElement('div');
      meta.id = 'hb-meta';

      if (author) {
        meta.appendChild(author);
      }
      if (date) {
        meta.appendChild(date);
      }

      left.appendChild(meta);
    }

    postamble.parentNode.removeChild(postamble);
  }

  var currentBox = document.createElement('div');
  currentBox.id = 'hb-topbar-current';
  var currentLabel = document.createElement('div');
  currentLabel.id = 'hb-topbar-current-label';
  currentLabel.textContent = '当前小节';
  var currentText = document.createElement('div');
  currentText.id = 'hb-current-heading';
  currentBox.appendChild(currentLabel);
  currentBox.appendChild(currentText);

  right.appendChild(currentBox);
  right.appendChild(controls);

  // 主题切换按钮：在浅色 / 深色模式之间切换
  var themeBtn = document.createElement('button');
  themeBtn.id = 'hb-theme-toggle';
  themeBtn.type = 'button';
  themeBtn.textContent = '夜间模式';
  themeBtn.addEventListener('click', function () {
    var next = currentTheme === 'dark' ? 'light' : 'dark';
    applyTheme(next || 'light');
  });
  themeToggleBtn = themeBtn;
  // 创建按钮后同步一次文案
  applyTheme(currentTheme || 'light');

  right.appendChild(themeBtn);

  body.insertBefore(topbar, content);

  // 移除正文中的原始 header
  if (header) {
    content.removeChild(header);
  }

  // 移动端目录开关按钮：点击时在 body 上切换 hb-toc-open 类
  var toc = content.querySelector('#table-of-contents');
  if (toc) {
    var tocBtn = document.createElement('button');
    tocBtn.id = 'hb-toc-toggle';
    tocBtn.type = 'button';
    tocBtn.textContent = '目录';
    tocBtn.setAttribute('aria-expanded', 'false');

    function syncTocVisibility() {
      var isMobile = window.innerWidth <= 1100;
      if (isMobile) {
        // 在窄屏下，根据 body 上的 hb-toc-open 控制目录显示
        var open = body.classList.contains('hb-toc-open');
        toc.style.display = open ? 'block' : 'none';
        tocBtn.setAttribute('aria-expanded', open ? 'true' : 'false');
      } else {
        // 桌面宽度：始终显示目录，清除内联样式和开关状态
        toc.style.display = '';
        body.classList.remove('hb-toc-open');
        tocBtn.setAttribute('aria-expanded', 'false');
      }
    }

    tocBtn.addEventListener('click', function () {
      // 切换 body 上的状态类，然后同步一次可见性
      body.classList.toggle('hb-toc-open');
      syncTocVisibility();
    });

    window.addEventListener('resize', syncTocVisibility);

    // 初始同步一次，确保在窄屏首次加载时目录与按钮状态一致
    syncTocVisibility();

    // 在移动端点击目录链接后自动收起下拉目录
    toc.addEventListener('click', function (ev) {
      var isMobile = window.innerWidth <= 1100;
      if (!isMobile) return;
      var target = ev.target;
      if (target && target.closest('a')) {
        body.classList.remove('hb-toc-open');
        syncTocVisibility();
      }
    });

    left.appendChild(tocBtn);
  }

  // 仅跟踪 h3/h4 作为当前小节标题
  var trackingHeadings = headings.filter(function (h) {
    var level = parseInt(h.tagName.substr(1), 10);
    return level === 3 || level === 4;
  });

  if (trackingHeadings.length) {
    var positions = [];

    function computePositions() {
      positions = trackingHeadings.map(function (el) {
        return {
          el: el,
          top: el.getBoundingClientRect().top + window.pageYOffset
        };
      });
    }

    function updateCurrentHeading() {
      if (!positions.length) return;
      var y = window.pageYOffset + 120;
      var current = null;
      for (var i = 0; i < positions.length; i++) {
        if (positions[i].top <= y) {
          current = positions[i];
        } else {
          break;
        }
      }
      if (!current) {
        currentText.textContent = '';
        return;
      }
      currentText.textContent = current.el.textContent.trim();
    }

    computePositions();
    updateCurrentHeading();

    window.addEventListener('scroll', function () {
      window.requestAnimationFrame(updateCurrentHeading);
    });

    window.addEventListener('resize', function () {
      computePositions();
      updateCurrentHeading();
    });
  }

  // 顶栏紧凑模式：仅根据滚动高度切换，避免边界来回抖动造成闪烁
  (function () {
    if (!topbar) return;

    var compact = false;
    var enterThreshold = 120; // 超过此高度后进入紧凑模式
    var exitThreshold = 60;   // 回到此高度内再恢复展开

    function setCompact(next) {
      if (next === compact) return;
      compact = next;
      if (compact) {
        topbar.classList.add('hb-topbar-compact');
      } else {
        topbar.classList.remove('hb-topbar-compact');
      }
    }

    function onScroll() {
      var y = window.pageYOffset || document.documentElement.scrollTop || 0;

      if (!compact && y > enterThreshold) {
        setCompact(true);
      } else if (compact && y < exitThreshold) {
        setCompact(false);
      }
    }

    window.addEventListener('scroll', onScroll, { passive: true });
    // 初次加载根据当前滚动位置设定一次状态
    onScroll();
  })();
});
