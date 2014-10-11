spk = pointProcess('times',[1:10]);

spk.window = [0 5;5 11];
raster(spk,'style','line')

spk.window = [5 11;0 5];
raster(spk,'style','line')

clear all

spk = pointProcess('times',[1:10]);

spk.window = [0 5;5 11];
raster(spk,'style','line')

spk.offset = 10;
raster(spk,'style','line')

% ???
spk.window = [5 11;0 5];
raster(spk,'style','line')

clear all

spk = pointProcess('times',[1:10]);

spk.window = [0 5;5 11];
raster(spk,'style','line')

spk.offset = 10;
raster(spk,'style','line')

spk.offset = 0;
raster(spk,'style','line')

% ???
spk.window = [5 11;0 5];
raster(spk,'style','line')

