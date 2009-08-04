var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 143</i>';
states['fold000001'] = false;
texts['fold000145'] = '<a href="javascript:fold(\'fold000145\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 145 to line 216</i>';
states['fold000145'] = false;
texts['fold000218'] = '<a href="javascript:fold(\'fold000218\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 218 to line 246</i>';
states['fold000218'] = false;
texts['fold000248'] = '<a href="javascript:fold(\'fold000248\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 248 to line 288</i>';
states['fold000248'] = false;
texts['fold000290'] = '<a href="javascript:fold(\'fold000290\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 290 to line 304</i>';
states['fold000290'] = false;
texts['fold000306'] = '<a href="javascript:fold(\'fold000306\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 306 to line 379</i>';
states['fold000306'] = false;
texts['fold000382'] = '<a href="javascript:fold(\'fold000382\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 382 to line 429</i>';
states['fold000382'] = false;
texts['fold000431'] = '<a href="javascript:fold(\'fold000431\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 431 to line 509</i>';
states['fold000431'] = false;
texts['fold000511'] = '<a href="javascript:fold(\'fold000511\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 511 to line 530</i>';
states['fold000511'] = false;
texts['fold000532'] = '<a href="javascript:fold(\'fold000532\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 532 to line 544</i>';
states['fold000532'] = false;
texts['fold000546'] = '<a href="javascript:fold(\'fold000546\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 546 to line 584</i>';
states['fold000546'] = false;

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
