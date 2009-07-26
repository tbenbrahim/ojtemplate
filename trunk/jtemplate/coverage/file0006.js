var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 68</i>';
states['fold000001'] = false;
texts['fold000070'] = '<a href="javascript:fold(\'fold000070\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 70 to line 113</i>';
states['fold000070'] = false;
texts['fold000115'] = '<a href="javascript:fold(\'fold000115\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 115 to line 165</i>';
states['fold000115'] = false;
texts['fold000169'] = '<a href="javascript:fold(\'fold000169\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 169 to line 228</i>';
states['fold000169'] = false;
texts['fold000230'] = '<a href="javascript:fold(\'fold000230\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 230 to line 232</i>';
states['fold000230'] = false;

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
