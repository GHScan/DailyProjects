#ifndef SJPEG_H
#define SJPEG_H

#include <cstdint>

#include <vector>

#include "Image.h"


void EncodeSJpeg(Image const &image, uint8_t *data, size_t &size, uint16_t quality);
void DecodeSJpeg(Image &image, uint8_t const *data, size_t size, uint16_t quality);


#endif