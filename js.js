var removeClass = function (el, className) {
  if (el.classList)
    el.classList.remove(className);
  else
    el.className = el.className.replace(new RegExp('(^|\\b)' + className.split(' ').join('|') + '(\\b|$)', 'gi'), ' ');
};
var addClass = function (el, className) {
    if (el.classList)
        el.classList.add(className);
    else
        el.className += ' ' + className;
};
window.onload = function() {
  var options = document.querySelectorAll('li');
  Array.prototype.forEach.call(options, function(opt){
    opt.onclick = function(){
      opt.parentNode.parentNode.querySelectorAll('.selection')[0].textContent = opt.getAttribute('data-value');
      removeClass(opt.parentNode.querySelectorAll('li.current')[0], 'current');
      addClass(opt, 'current');
    };
  });
}
