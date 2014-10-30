#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define SOURCE 	'@'

// A cell on the map

typedef struct cell_s
{
        int     x;
        int     y;
} cell_t;

typedef struct map_s
{
	int	size;		// length of the edge of the square map
	int* 	map;		// map, each cell contains the height of the soil
	float* 	flooding_map;	// each cell contains the quantity of water left on the soil by the flooding  
} map_t;

int calls;

int 	read_map(const char * _filename, map_t* _map);
void 	print_map(const map_t* _map, cell_t _source);
void	print_flooding_map(const map_t* _map, cell_t _source);
void 	free_map(map_t _map);

int 	get_map_value(map_t _map, cell_t _point);
float 	get_map_flooding_value(map_t _map, cell_t _point);
void 	set_map_flooding(map_t _map, cell_t _point, float _water_quantity);

void 	compute_flooding(cell_t _source, cell_t _cell, map_t  _map, float _flooding_size);

int main(int argc, char ** argv)
{
	cell_t source;
	map_t map;
	int flooding_size;

	if(argc < 5)
	{
		printf("Usage: %s [map filename] [source x position] [source y position] [flooding size]\n", argv[0]);
		return -1;
	}

	if(read_map(argv[1], &map))
		return -1;

	source.x = atoi(argv[2]);
	source.y = atoi(argv[3]);
	flooding_size = atoi(argv[4]);
	
	if(source.x < 0 || source.x >= map.size || source.y < 0 || source.y >= map.size || flooding_size < 0)
	{
		printf("Invalid parameters\n");
		return -1;
	}
	
	//print_map(&map, source);
	//printf("\n");

	double start_time = omp_get_wtime();
	#pragma omp parallel
	{
		#pragma omp single		
		compute_flooding(source, source, map, flooding_size);
	}
	printf("Computed flooding for %dx%d map\nCalls: %11d\nTime: %12.4lf\n", map.size, map.size, calls, omp_get_wtime()-start_time);

	//print_flooding_map(&map, source);
	free_map(map);

	return 0;
}

void compute_flooding(cell_t _source, cell_t _cell, map_t  _map, float _flooding_size)
{
	calls++;
	int i;
	int x_offset[4] = {0, 1, 0, -1};
	int y_offset[4] = {1, 0, -1, 0};
	int floodable_cells = 1;
	float cur_flooding_size;
	int cur_height = get_map_value(_map, _cell);

	if(_flooding_size < 1)
		return;	
	
	for(i=0; i<4; i++)
	{
		int dest_height;

		cell_t dest = {.x = _cell.x + x_offset[i], .y = _cell.y + y_offset[i]};

		if(dest.x < 0 || dest.x >= _map.size || dest.y < 0 || dest.y >= _map.size || (_cell.x == _source.x && _cell.y == _source.y))
			continue;
		
		dest_height = get_map_value(_map, dest);

		if(dest_height > cur_height)
			continue;
		
		floodable_cells ++;
	}

	cur_flooding_size = _flooding_size / floodable_cells;
	if(cur_flooding_size < 0.1)
		cur_flooding_size = 0;

	for(i=0; i<4; i++)
	{
		int dest_height;
		cell_t dest = {.x = _cell.x + x_offset[i], .y = _cell.y + y_offset[i]};

		if(dest.x < 0 || dest.x >= _map.size || dest.y < 0 || dest.y >= _map.size || (_cell.x == _source.x && _cell.y == _source.y))
			continue;
		
		dest_height = get_map_value(_map, dest);

		if(dest_height > cur_height)
			continue;
		
		#pragma omp task
		compute_flooding(_cell, dest, _map, cur_flooding_size);
	}

	set_map_flooding(_map, _cell, cur_flooding_size);
}

int get_map_value(map_t _map, cell_t _point)
{
	/*	    x
		0 1 2 3 4 
		1
	      y 2
		3
		4
	*/

	return _map.map[_point.x + _point.y * _map.size];
}

float get_map_flooding_value(map_t _map, cell_t _point)
{
	/*	    x
		0 1 2 3 4 
		1
	      y 2
		3
		4
	*/

	return _map.flooding_map[_point.x + _point.y * _map.size];
}
void set_map_flooding(map_t _map, cell_t _point, float  _water_quantity)
{
	/*	    x
		0 1 2 3 4 
		1
	      y 2
		3
		4
	*/

	#pragma omp atomic	
	_map.flooding_map[_point.x + _point.y * _map.size] += _water_quantity;

	return;
}


int read_map(const char * _filename, map_t* _map)
{
	FILE* fp;
	int i;

	fp = fopen(_filename, "r");
	if(fp == NULL)
	{
		printf("Cannot open file %s\n", _filename);
		return -1;
	}

	fscanf(fp, "%d", &_map->size);

	_map->map = (int*)calloc(_map->size * _map->size, sizeof(int));
	if(_map->map == NULL)
	{
		printf("Cannot allocate memory\n" );
		return -1;
	}

	_map->flooding_map = (float*)calloc(_map->size * _map->size, sizeof(float));
	if(_map->flooding_map == NULL)
	{
		printf("Cannot allocate memory\n");
		free(_map->map);
		return -1;
	}

	i = -1;
	while(!feof(fp) && i < _map->size * _map->size -1)
	{
		fscanf(fp, "%d", &(_map->map[++i]));
	}
	
	fclose(fp);

	return 0;
}

void print_map(const map_t* _map, cell_t _source)
{
	int i, j;
	
	if(_map == NULL || _map->map == NULL || _map->size <= 0)
		return;

	printf("%c = SOURCE\n\n", SOURCE);

	for(i=_map->size -1; i>=0; i--)
	{
		for(j=0; j<_map->size; j++)
		{
			if(j == _source.x && i == _source.y)
				printf("@ ");
			else
				printf("%d ", _map->map[i*_map->size + j]);
		}
		printf("\t%d\n", i);
	}

	return;
}

void print_flooding_map(const map_t* _map, cell_t _source)
{
	int i, j;
	
	if(_map == NULL || _map->flooding_map == NULL || _map->size <= 0)
		return;

	printf("%c = SOURCE\n.  = NOT FLOODED\nS = FLOODED\n\n", SOURCE);
	for(i=_map->size -1; i>=0; i--)
	{
		for(j=0; j<_map->size; j++)
		{
			if(j == _source.x && i == _source.y)
				printf("@ ");
			else
				printf("%c ", _map->flooding_map[i*_map->size + j] ? 'S' : '.');
		}
		printf(" %d\n", i);
	}

	return;
}

void free_map(map_t _map)
{
	free(_map.map);
	free(_map.flooding_map);
}

