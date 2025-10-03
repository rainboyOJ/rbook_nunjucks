/**
 * 暗黑模式切换功能
 */

// 等待DOM加载完成
document.addEventListener('DOMContentLoaded', function() {
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
