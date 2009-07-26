var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 31</i>';
states['fold000001'] = false;
texts['fold000034'] = '<a href="javascript:fold(\'fold000034\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 34 to line 38</i>';
states['fold000034'] = false;
texts['fold000041'] = '<a href="javascript:fold(\'fold000041\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 41 to line 48</i>';
states['fold000041'] = false;
texts['fold000050'] = '<a href="javascript:fold(\'fold000050\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 50 to line 50</i>';
states['fold000050'] = false;
texts['fold000052'] = '<a href="javascript:fold(\'fold000052\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 52 to line 72</i>';
states['fold000052'] = false;
texts['fold000074'] = '<a href="javascript:fold(\'fold000074\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 74 to line 75</i>';
states['fold000074'] = false;
texts['fold000082'] = '<a href="javascript:fold(\'fold000082\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 82 to line 85</i>';
states['fold000082'] = false;
texts['fold000087'] = '<a href="javascript:fold(\'fold000087\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 87 to line 97</i>';
states['fold000087'] = false;
texts['fold000099'] = '<a href="javascript:fold(\'fold000099\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 99 to line 104</i>';
states['fold000099'] = false;
texts['fold000107'] = '<a href="javascript:fold(\'fold000107\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 107 to line 118</i>';
states['fold000107'] = false;
texts['fold000120'] = '<a href="javascript:fold(\'fold000120\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 120 to line 127</i>';
states['fold000120'] = false;
texts['fold000129'] = '<a href="javascript:fold(\'fold000129\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 129 to line 145</i>';
states['fold000129'] = false;
texts['fold000150'] = '<a href="javascript:fold(\'fold000150\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 150 to line 172</i>';
states['fold000150'] = false;
texts['fold000174'] = '<a href="javascript:fold(\'fold000174\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 174 to line 183</i>';
states['fold000174'] = false;
texts['fold000185'] = '<a href="javascript:fold(\'fold000185\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 185 to line 195</i>';
states['fold000185'] = false;
texts['fold000197'] = '<a href="javascript:fold(\'fold000197\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 197 to line 197</i>';
states['fold000197'] = false;
texts['fold000199'] = '<a href="javascript:fold(\'fold000199\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 199 to line 217</i>';
states['fold000199'] = false;
texts['fold000219'] = '<a href="javascript:fold(\'fold000219\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 219 to line 219</i>';
states['fold000219'] = false;
texts['fold000221'] = '<a href="javascript:fold(\'fold000221\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 221 to line 222</i>';
states['fold000221'] = false;

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
