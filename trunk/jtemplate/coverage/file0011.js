var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 6</i>';
states['fold000001'] = false;
texts['fold000011'] = '<a href="javascript:fold(\'fold000011\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 11 to line 11</i>';
states['fold000011'] = false;
texts['fold000013'] = '<a href="javascript:fold(\'fold000013\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 13 to line 13</i>';
states['fold000013'] = false;
texts['fold000020'] = '<a href="javascript:fold(\'fold000020\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 20 to line 21</i>';
states['fold000020'] = false;
texts['fold000025'] = '<a href="javascript:fold(\'fold000025\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 25 to line 48</i>';
states['fold000025'] = false;
texts['fold000050'] = '<a href="javascript:fold(\'fold000050\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 50 to line 66</i>';
states['fold000050'] = false;
texts['fold000068'] = '<a href="javascript:fold(\'fold000068\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 68 to line 74</i>';
states['fold000068'] = false;
texts['fold000076'] = '<a href="javascript:fold(\'fold000076\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 76 to line 103</i>';
states['fold000076'] = false;
texts['fold000105'] = '<a href="javascript:fold(\'fold000105\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 105 to line 133</i>';
states['fold000105'] = false;
texts['fold000135'] = '<a href="javascript:fold(\'fold000135\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 135 to line 175</i>';
states['fold000135'] = false;
texts['fold000177'] = '<a href="javascript:fold(\'fold000177\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 177 to line 205</i>';
states['fold000177'] = false;

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
