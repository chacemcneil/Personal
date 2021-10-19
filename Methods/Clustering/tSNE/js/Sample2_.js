(function($) {
    $(document).ready(function() {
	
	$('#Sample2_').scianimator({
	    'images': ['images/Sample2_1.png', 'images/Sample2_2.png', 'images/Sample2_3.png', 'images/Sample2_4.png', 'images/Sample2_5.png', 'images/Sample2_6.png', 'images/Sample2_7.png', 'images/Sample2_8.png', 'images/Sample2_9.png', 'images/Sample2_10.png', 'images/Sample2_11.png', 'images/Sample2_12.png', 'images/Sample2_13.png', 'images/Sample2_14.png', 'images/Sample2_15.png', 'images/Sample2_16.png', 'images/Sample2_17.png', 'images/Sample2_18.png', 'images/Sample2_19.png', 'images/Sample2_20.png'],
	    'width': 480,
	    'delay': 500,
	    'loopMode': 'none',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed']
	});
	$('#Sample2_').scianimator('play');
    });
})(jQuery);
