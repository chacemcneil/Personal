(function($) {
    $(document).ready(function() {
	
	$('#Random').scianimator({
	    'images': ['images/Random1.png', 'images/Random2.png', 'images/Random3.png', 'images/Random4.png', 'images/Random5.png', 'images/Random6.png', 'images/Random7.png', 'images/Random8.png', 'images/Random9.png', 'images/Random10.png', 'images/Random11.png', 'images/Random12.png', 'images/Random13.png', 'images/Random14.png', 'images/Random15.png', 'images/Random16.png', 'images/Random17.png', 'images/Random18.png', 'images/Random19.png', 'images/Random20.png'],
	    'width': 480,
	    'delay': 500,
	    'loopMode': 'none',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed']
	});
	$('#Random').scianimator('play');
    });
})(jQuery);
