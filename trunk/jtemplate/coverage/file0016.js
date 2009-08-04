var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 621</i>';
states['fold000001'] = false;
texts['fold000623'] = '<a href="javascript:fold(\'fold000623\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 623 to line 652</i>';
states['fold000623'] = false;
texts['fold000654'] = '<a href="javascript:fold(\'fold000654\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 654 to line 865</i>';
states['fold000654'] = false;
texts['fold000868'] = '<a href="javascript:fold(\'fold000868\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 868 to line 872</i>';
states['fold000868'] = false;
texts['fold000874'] = '<a href="javascript:fold(\'fold000874\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 874 to line 987</i>';
states['fold000874'] = false;
texts['fold000991'] = '<a href="javascript:fold(\'fold000991\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 991 to line 1268</i>';
states['fold000991'] = false;
texts['fold001271'] = '<a href="javascript:fold(\'fold001271\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1271 to line 1408</i>';
states['fold001271'] = false;
texts['fold001412'] = '<a href="javascript:fold(\'fold001412\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1412 to line 1416</i>';
states['fold001412'] = false;
texts['fold001418'] = '<a href="javascript:fold(\'fold001418\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1418 to line 1422</i>';
states['fold001418'] = false;
texts['fold001425'] = '<a href="javascript:fold(\'fold001425\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1425 to line 1437</i>';
states['fold001425'] = false;
texts['fold001442'] = '<a href="javascript:fold(\'fold001442\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1442 to line 1467</i>';
states['fold001442'] = false;
texts['fold001471'] = '<a href="javascript:fold(\'fold001471\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1471 to line 1497</i>';
states['fold001471'] = false;
texts['fold001500'] = '<a href="javascript:fold(\'fold001500\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1500 to line 1529</i>';
states['fold001500'] = false;

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
