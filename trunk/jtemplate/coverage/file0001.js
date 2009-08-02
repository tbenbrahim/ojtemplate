var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 89</i>';
states['fold000001'] = false;
texts['fold000091'] = '<a href="javascript:fold(\'fold000091\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 91 to line 91</i>';
states['fold000091'] = false;
texts['fold000093'] = '<a href="javascript:fold(\'fold000093\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 93 to line 113</i>';
states['fold000093'] = false;
texts['fold000119'] = '<a href="javascript:fold(\'fold000119\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 119 to line 121</i>';
states['fold000119'] = false;
texts['fold000123'] = '<a href="javascript:fold(\'fold000123\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 123 to line 123</i>';
states['fold000123'] = false;
texts['fold000126'] = '<a href="javascript:fold(\'fold000126\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 126 to line 126</i>';
states['fold000126'] = false;

function fold(id) {
  tmp = document.all[id].innerHTML;
  document.all[id].innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
