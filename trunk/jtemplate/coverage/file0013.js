var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 35</i>';
states['fold000001'] = false;
texts['fold000037'] = '<a href="javascript:fold(\'fold000037\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 37 to line 37</i>';
states['fold000037'] = false;
texts['fold000039'] = '<a href="javascript:fold(\'fold000039\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 39 to line 54</i>';
states['fold000039'] = false;
texts['fold000056'] = '<a href="javascript:fold(\'fold000056\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 56 to line 61</i>';
states['fold000056'] = false;
texts['fold000063'] = '<a href="javascript:fold(\'fold000063\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 63 to line 86</i>';
states['fold000063'] = false;
texts['fold000088'] = '<a href="javascript:fold(\'fold000088\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 88 to line 89</i>';
states['fold000088'] = false;
texts['fold000092'] = '<a href="javascript:fold(\'fold000092\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 92 to line 93</i>';
states['fold000092'] = false;
texts['fold000096'] = '<a href="javascript:fold(\'fold000096\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 96 to line 104</i>';
states['fold000096'] = false;
texts['fold000106'] = '<a href="javascript:fold(\'fold000106\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 106 to line 107</i>';
states['fold000106'] = false;
texts['fold000112'] = '<a href="javascript:fold(\'fold000112\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 112 to line 122</i>';
states['fold000112'] = false;
texts['fold000124'] = '<a href="javascript:fold(\'fold000124\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 124 to line 126</i>';
states['fold000124'] = false;
texts['fold000130'] = '<a href="javascript:fold(\'fold000130\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 130 to line 140</i>';
states['fold000130'] = false;
texts['fold000146'] = '<a href="javascript:fold(\'fold000146\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 146 to line 149</i>';
states['fold000146'] = false;
texts['fold000151'] = '<a href="javascript:fold(\'fold000151\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 151 to line 156</i>';
states['fold000151'] = false;
texts['fold000158'] = '<a href="javascript:fold(\'fold000158\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 158 to line 248</i>';
states['fold000158'] = false;
texts['fold000250'] = '<a href="javascript:fold(\'fold000250\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 250 to line 269</i>';
states['fold000250'] = false;
texts['fold000271'] = '<a href="javascript:fold(\'fold000271\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 271 to line 279</i>';
states['fold000271'] = false;
texts['fold000281'] = '<a href="javascript:fold(\'fold000281\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 281 to line 289</i>';
states['fold000281'] = false;
texts['fold000291'] = '<a href="javascript:fold(\'fold000291\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 291 to line 299</i>';
states['fold000291'] = false;
texts['fold000301'] = '<a href="javascript:fold(\'fold000301\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 301 to line 303</i>';
states['fold000301'] = false;
texts['fold000305'] = '<a href="javascript:fold(\'fold000305\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 305 to line 305</i>';
states['fold000305'] = false;
texts['fold000311'] = '<a href="javascript:fold(\'fold000311\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 311 to line 321</i>';
states['fold000311'] = false;
texts['fold000323'] = '<a href="javascript:fold(\'fold000323\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 323 to line 361</i>';
states['fold000323'] = false;

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
