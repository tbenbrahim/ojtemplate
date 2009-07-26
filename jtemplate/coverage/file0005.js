var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 97</i>';
states['fold000001'] = false;
texts['fold000099'] = '<a href="javascript:fold(\'fold000099\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 99 to line 163</i>';
states['fold000099'] = false;
texts['fold000165'] = '<a href="javascript:fold(\'fold000165\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 165 to line 171</i>';
states['fold000165'] = false;
texts['fold000173'] = '<a href="javascript:fold(\'fold000173\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 173 to line 209</i>';
states['fold000173'] = false;
texts['fold000211'] = '<a href="javascript:fold(\'fold000211\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 211 to line 223</i>';
states['fold000211'] = false;
texts['fold000225'] = '<a href="javascript:fold(\'fold000225\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 225 to line 292</i>';
states['fold000225'] = false;
texts['fold000295'] = '<a href="javascript:fold(\'fold000295\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 295 to line 316</i>';
states['fold000295'] = false;
texts['fold000319'] = '<a href="javascript:fold(\'fold000319\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 319 to line 338</i>';
states['fold000319'] = false;
texts['fold000340'] = '<a href="javascript:fold(\'fold000340\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 340 to line 371</i>';
states['fold000340'] = false;
texts['fold000373'] = '<a href="javascript:fold(\'fold000373\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 373 to line 402</i>';
states['fold000373'] = false;
texts['fold000404'] = '<a href="javascript:fold(\'fold000404\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 404 to line 404</i>';
states['fold000404'] = false;

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
