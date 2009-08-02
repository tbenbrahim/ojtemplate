var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 84</i>';
states['fold000001'] = false;
texts['fold000086'] = '<a href="javascript:fold(\'fold000086\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 86 to line 86</i>';
states['fold000086'] = false;
texts['fold000088'] = '<a href="javascript:fold(\'fold000088\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 88 to line 131</i>';
states['fold000088'] = false;
texts['fold000133'] = '<a href="javascript:fold(\'fold000133\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 133 to line 183</i>';
states['fold000133'] = false;
texts['fold000187'] = '<a href="javascript:fold(\'fold000187\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 187 to line 244</i>';
states['fold000187'] = false;
texts['fold000246'] = '<a href="javascript:fold(\'fold000246\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 246 to line 248</i>';
states['fold000246'] = false;

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
