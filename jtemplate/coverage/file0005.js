var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 103</i>';
states['fold000001'] = false;
texts['fold000105'] = '<a href="javascript:fold(\'fold000105\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 105 to line 171</i>';
states['fold000105'] = false;
texts['fold000173'] = '<a href="javascript:fold(\'fold000173\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 173 to line 179</i>';
states['fold000173'] = false;
texts['fold000181'] = '<a href="javascript:fold(\'fold000181\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 181 to line 219</i>';
states['fold000181'] = false;
texts['fold000221'] = '<a href="javascript:fold(\'fold000221\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 221 to line 234</i>';
states['fold000221'] = false;
texts['fold000236'] = '<a href="javascript:fold(\'fold000236\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 236 to line 306</i>';
states['fold000236'] = false;
texts['fold000309'] = '<a href="javascript:fold(\'fold000309\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 309 to line 331</i>';
states['fold000309'] = false;
texts['fold000334'] = '<a href="javascript:fold(\'fold000334\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 334 to line 354</i>';
states['fold000334'] = false;
texts['fold000356'] = '<a href="javascript:fold(\'fold000356\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 356 to line 387</i>';
states['fold000356'] = false;
texts['fold000390'] = '<a href="javascript:fold(\'fold000390\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 390 to line 390</i>';
states['fold000390'] = false;
texts['fold000393'] = '<a href="javascript:fold(\'fold000393\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 393 to line 402</i>';
states['fold000393'] = false;
texts['fold000404'] = '<a href="javascript:fold(\'fold000404\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 404 to line 433</i>';
states['fold000404'] = false;
texts['fold000435'] = '<a href="javascript:fold(\'fold000435\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 435 to line 435</i>';
states['fold000435'] = false;

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
