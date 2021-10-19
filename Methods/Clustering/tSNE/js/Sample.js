(function($) {
    $(document).ready(function() {
	
	$('#Sample').scianimator({
	    'images': ['images/Sample1.png', 'images/Sample2.png', 'images/Sample3.png', 'images/Sample4.png', 'images/Sample5.png', 'images/Sample6.png', 'images/Sample7.png', 'images/Sample8.png', 'images/Sample9.png', 'images/Sample10.png', 'images/Sample11.png', 'images/Sample12.png', 'images/Sample13.png', 'images/Sample14.png', 'images/Sample15.png', 'images/Sample16.png', 'images/Sample17.png', 'images/Sample18.png', 'images/Sample19.png', 'images/Sample20.png'],
	    'width': 480,
	    'delay': 500,
	    'loopMode': 'none',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed']
	});
	$('#Sample').scianimator('play');
    });
})(jQuery);
