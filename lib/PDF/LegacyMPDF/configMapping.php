<?php
return $configOptions = [
    'mode' => $options['mode'] ?? '',
    'page_size' => $options['page_size'] ?? 'A4',
    'default_font_size' => $options['fontSize'] ?? 11,
    'default_font' => $options['font'] ?? 'DejaVuSansCondensed',
    'margin_left' => $options['margin_left'] ?? 15,
    'margin_right' => $options['margin_right'] ?? 15,
    'margin_top' => $options['margin_top'] ?? 16,
    'margin_bottom' => $options['margin_bottom'] ?? 16,
    'margin_header' => $options['margin_header'] ?? 9,
    'margin_footer' => $options['margin_footer'] ?? 9,
    'orientation' => $options['orientation'] ?? 'P',
    'unit' => $options['unit'] ?? 'mm'
];
