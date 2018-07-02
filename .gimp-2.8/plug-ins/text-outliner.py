#!/usr/bin/env python

# This is a GIMP script written in Python for putting an outline around text (or indeed anything
# else).
#
# To use this script, make sure that the selected layer is the one that you want to outline, then
# choose "Text Outliner" from the "Decor" submenu of the "Filters" menu
#
# You will then be able to set various parameters for the outline. They are:
#		Colour - what colour you want the outline to be
#		Thickness - thickness of the outline (in pixels)
#		Feather - how soft you want the edge of the outline to be
#
# The script will create a new, transparent layer underneath your selected layer, and draw the
# outline into that new layer.

from gimpfu import *

# Adds a new layer beneath the given layer. Return value is the new layer
def add_new_layer_beneath(image, layer):
	# Get the layer position.
	pos = 0;
	for i in range(len(image.layers)):
		if(image.layers[i] == layer):
			pos = i

	if image.base_type is RGB:
		type = RGBA_IMAGE
	else:
		type = GRAYA_IMAGE

	# Add a new layer below the selected one
	new_layer = gimp.Layer(image, "text outline", image.width, image.height, type, 100, NORMAL_MODE)
	image.add_layer(new_layer, pos+1)
	return new_layer

# Selects the contents of the given layer, then grows it by "thickness"
# and feathers it by "feather" pixels
def create_selection(image, layer, thickness, feather):
	# Select the text
	pdb.gimp_selection_layer_alpha(layer)

	# Grow the selection
	pdb.gimp_selection_grow(image, thickness)

	# Feather it
	if (feather > 0):
		pdb.gimp_selection_feather(image, feather)
	return

# Fills the current selection using the given colour, painting onto the
# given layer.
def fill_selection(layer, colour):
	# Cache existing foreground colour
	old_fg = pdb.gimp_palette_get_foreground()
	# Set foreground colour
	pdb.gimp_palette_set_foreground(colour)
	# Fill the selection
	pdb.gimp_bucket_fill(layer, 0, 0, 100, 0, 0, 1, 1)
	# Restore cached foreground colour
	pdb.gimp_palette_set_foreground(old_fg)
	return

# our script
def add_text_outline(image, layer, colour, thickness, feather) :
	gimp.progress_init("Drawing outline around text")
	new_layer = add_new_layer_beneath(image, layer)
	gimp.progress_update(33)
	create_selection(image, layer, thickness, feather)
	gimp.progress_update(66)
	fill_selection(new_layer, colour)
	gimp.progress_update(100)
	return

# This is the plugin registration function
register(
	"text_outliner",
	"Text Outliner",
	"Will draw an outline around text (or indeed anything else). The outline is drawn to a separate layer underneath the selected layer.",
	"Pete Nu",
	"Pete Nu",
	"Feb 2015",
	"<Image>/Filters/Decor/Text Outliner",
	"*",
	[
		(PF_COLOUR, 'outline_colour', 'Colour', (0, 0, 0)),
		(PF_INT, 'outline_thickness', 'Thickness', 6),
		(PF_INT, 'outline_featheriness', 'Feather', 7)
	],
	[],
	add_text_outline)

main()
