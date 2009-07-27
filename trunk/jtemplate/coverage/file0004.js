var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 14</i>';
states['fold000001'] = false;
texts['fold000016'] = '<a href="javascript:fold(\'fold000016\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 16 to line 187</i>';
states['fold000016'] = false;
texts['fold000197'] = '<a href="javascript:fold(\'fold000197\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 197 to line 197</i>';
states['fold000197'] = false;
texts['fold000201'] = '<a href="javascript:fold(\'fold000201\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 201 to line 201</i>';
states['fold000201'] = false;
texts['fold000205'] = '<a href="javascript:fold(\'fold000205\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 205 to line 205</i>';
states['fold000205'] = false;
texts['fold000207'] = '<a href="javascript:fold(\'fold000207\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 207 to line 207</i>';
states['fold000207'] = false;
texts['fold000210'] = '<a href="javascript:fold(\'fold000210\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 210 to line 210</i>';
states['fold000210'] = false;
texts['fold000219'] = '<a href="javascript:fold(\'fold000219\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 219 to line 219</i>';
states['fold000219'] = false;
texts['fold000224'] = '<a href="javascript:fold(\'fold000224\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 224 to line 224</i>';
states['fold000224'] = false;
texts['fold000230'] = '<a href="javascript:fold(\'fold000230\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 230 to line 231</i>';
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
