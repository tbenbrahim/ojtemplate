var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 41</i>';
states['fold000001'] = false;
texts['fold000044'] = '<a href="javascript:fold(\'fold000044\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 44 to line 65</i>';
states['fold000044'] = false;
texts['fold000067'] = '<a href="javascript:fold(\'fold000067\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 67 to line 92</i>';
states['fold000067'] = false;
texts['fold000095'] = '<a href="javascript:fold(\'fold000095\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 95 to line 122</i>';
states['fold000095'] = false;
texts['fold000124'] = '<a href="javascript:fold(\'fold000124\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 124 to line 163</i>';
states['fold000124'] = false;
texts['fold000165'] = '<a href="javascript:fold(\'fold000165\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 165 to line 185</i>';
states['fold000165'] = false;
texts['fold000189'] = '<a href="javascript:fold(\'fold000189\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 189 to line 195</i>';
states['fold000189'] = false;
texts['fold000197'] = '<a href="javascript:fold(\'fold000197\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 197 to line 284</i>';
states['fold000197'] = false;
texts['fold000286'] = '<a href="javascript:fold(\'fold000286\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 286 to line 289</i>';
states['fold000286'] = false;
texts['fold000291'] = '<a href="javascript:fold(\'fold000291\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 291 to line 318</i>';
states['fold000291'] = false;
texts['fold000320'] = '<a href="javascript:fold(\'fold000320\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 320 to line 327</i>';
states['fold000320'] = false;
texts['fold000329'] = '<a href="javascript:fold(\'fold000329\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 329 to line 412</i>';
states['fold000329'] = false;
texts['fold000414'] = '<a href="javascript:fold(\'fold000414\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 414 to line 414</i>';
states['fold000414'] = false;
texts['fold000416'] = '<a href="javascript:fold(\'fold000416\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 416 to line 423</i>';
states['fold000416'] = false;

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
