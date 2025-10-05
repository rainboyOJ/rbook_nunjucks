
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

/**
 * 暗黑模式切换功能
 */
// 等待DOM加载完成
document.addEventListener('DOMContentLoaded', function() {
  init_copy();
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
