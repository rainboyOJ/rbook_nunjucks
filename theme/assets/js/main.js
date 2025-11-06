
const tooltipTriggerList = document.querySelectorAll('[data-bs-toggle="tooltip"]')
const tooltipList = [...tooltipTriggerList].map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl))

function init_copy(params) {

    //copy to clipboard
    document.querySelectorAll('.zeroclipboard-container').forEach( function(clipContainer){
        clipContainer.addEventListener('click', function(event) {
            if( clipContainer.classList.contains('copied')) return;

            // console.log(clipContainer.parentNode.parentNode)
            let text = clipContainer.parentNode.parentNode.querySelector('pre > code').textContent
          clipContainer.classList.add('copied');
            try {
                // 但 navigator.clipboard.writeText 只能在 HTTPS 或 localhost 下工作，并且需要用户交互（如点击），否则会报错。
                navigator.clipboard.writeText(text).then( ()=>{
                    setTimeout( ()=> clipContainer.classList.remove('copied'),1500)
                })
            }
            catch(err) {
                // alert('failed to copy!',err)
                fallbackCopyText(text,clipContainer)
                setTimeout(() => clipContainer.classList.remove('copied'), 1500)
            }
        })
    })
}

function fallbackCopyText(text, clipContainer) {
  // 创建一个临时button
  const tempBtn = document.createElement('button');
  tempBtn.style.position = 'absolute';
  tempBtn.style.left = '-9999px';
  document.body.appendChild(tempBtn);

  // 初始化 clipboard.js
  const clipboard = new ClipboardJS(tempBtn, {
    text: function() {
      return text;
    }
  });

  clipboard.on('success', function() {
    clipContainer.classList.add('copied');
    setTimeout(() => clipContainer.classList.remove('copied'), 1500);
    clipboard.destroy();
    document.body.removeChild(tempBtn);
  });

  clipboard.on('error', function() {
    alert('failed to copy!');
    clipboard.destroy();
    document.body.removeChild(tempBtn);
  });

  // 触发点击
  tempBtn.click();
  setTimeout(() => {
    // remove tempBtn
    document.body.removeChild(tempBtn);
  }, 5* 1000);
}


// code tab switch
function init_code_tab_switch() {
  // 找到页面上所有的 .code-tabs 容器
  // 这样即使页面上有多个 tab 组，脚本也能全部处理
  const allCodeTabs = document.querySelectorAll('.code-tabs');

  allCodeTabs.forEach(container => {

    // 定义一个函数来更新所有相关 code-block 的显示状态
    const updateTabs = (groupName) => {
      if (!groupName) return;

      // 找到这个容器内所有同名的 radio 按钮
      const radiosInGroup = container.querySelectorAll(`input[type="radio"][name="${groupName}"]`);

      radiosInGroup.forEach(radio => {
        // 找到 radio 按钮紧邻的下一个兄弟元素
        const nextEl = radio.nextElementSibling;

        // 关键检查：
        // 1. nextEl 必须存在
        // 2. nextEl 必须是一个 DIV 元素 (这会忽略 <ul> 里的 radio，因为它们的兄弟是 <label>)
        if (nextEl && nextEl.tagName === 'DIV') {
          if (radio.checked) {
            // 如果 radio 被选中，显示这个 DIV (移除 display: none)
            nextEl.style.display = ''; // 或者 'block'
          } else {
            // 如果 radio 未被选中，隐藏这个 DIV
            nextEl.style.display = 'none';
          }
        }
      });
    };

    // 使用事件委托，监听整个容器的 'change' 事件
    container.addEventListener('change', (event) => {
      // 确保事件是由我们关心的 radio 按钮触发的
      if (event.target.type === 'radio' && event.target.name) {
        // 得到 radio 按钮的 id 
        let id = event.target.id;
        console.log('change', event.target.name, 'id: ',id);
        updateTabs(event.target.name);
      }
    });

    // 初始化：页面加载时，立即运行一次更新
    // 这会隐藏所有未被 'checked' 的 code-block
    const initialRadios = container.querySelectorAll('input[type="radio"]:checked');
    initialRadios.forEach(radio => {
      updateTabs(radio.name);
    });
  });
}


/**
 * 暗黑模式切换功能
 */
// 等待DOM加载完成
document.addEventListener('DOMContentLoaded', function() {
  init_copy();
  init_code_tab_switch();
  // 获取所有暗黑模式切换按钮
  const darkModeButtons = document.querySelectorAll('.J_darkMode');
  
  // 从localStorage获取保存的主题设置，如果没有则使用默认值
  const savedTheme = localStorage.getItem('theme') || 'auto';
  
  // 设置当前主题
  setTheme(savedTheme);
  
  // 为每个按钮添加事件监听器
  darkModeButtons.forEach(button => {
    // 如果按钮的值与保存的主题匹配，则选中它
    if (button.value === savedTheme) {
      button.checked = true;
    }
    
    // 添加change事件监听器
    button.addEventListener('change', function() {
      if (this.checked) {
        setTheme(this.value);
        // 保存选择到localStorage
        localStorage.setItem('theme', this.value);
      }
    });
  });
  
  // 应用主题
  function setTheme(theme) {
    // 更新html元素的data-darkmode属性
    document.documentElement.setAttribute('data-darkmode', theme);
  }
  
  // 监听系统主题变化
  if (window.matchMedia) {
    const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
    mediaQuery.addEventListener('change', function(e) {
      // 只有在选择auto模式时才响应系统主题变化
      if (document.documentElement.getAttribute('data-darkmode') === 'auto') {
        if (e.matches) {
          document.documentElement.classList.remove('light');
          document.documentElement.classList.add('dark');
        } else {
          document.documentElement.classList.remove('dark');
          document.documentElement.classList.add('light');
        }
      }
    });
  }
});



function updateTocActive() {
  const headings = Array.from(document.querySelectorAll('h1, h2, h3, h4, h5, h6'));
  const tocLinks = Array.from(document.querySelectorAll('.table-of-contents a'));
  let activeId = '';
  const scrollY = window.scrollY || window.pageYOffset;
  for (let i = 0; i < headings.length; i++) {
    const h = headings[i];
    if (h.offsetTop - 80 <= scrollY) {
      activeId = h.id;
    }
  }
  activeId = decodeURIComponent(activeId);
  console.log('activeId', activeId);
  tocLinks.forEach(link => {
    let linkTo = decodeURIComponent(link.getAttribute('href').replace(/^#/, ''));
    console.log('-->', linkTo, activeId);
    if (linkTo === activeId) {
      link.classList.add('active');
    } else {
      link.classList.remove('active');
    }
  });
}


window.addEventListener('scroll', updateTocActive);
document.addEventListener('DOMContentLoaded', updateTocActive);