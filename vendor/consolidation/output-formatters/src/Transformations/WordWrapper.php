<?php

namespace Consolidation\OutputFormatters\Transformations;

use Consolidation\OutputFormatters\Transformations\Wrap\CalculateWidths;
use Consolidation\OutputFormatters\Transformations\Wrap\ColumnWidths;
use Symfony\Component\Console\Helper\TableStyle;

class WordWrapper
{
    protected $width;
    protected $minimumWidths;

    // For now, hardcode these to match what the Symfony Table helper does.
    // Note that these might actually need to be adjusted depending on the
    // table style.
    protected $extraPaddingAtBeginningOfLine = 0;
    protected $extraPaddingAtEndOfLine = 0;
    protected $paddingInEachCell = 3;

    public function __construct($width)
    {
        $this->width = $width;
        $this->minimumWidths = new ColumnWidths();
    }

    /**
     * Calculate our padding widths from the specified table style.
     * @param TableStyle $style
     */
    public function setPaddingFromStyle(TableStyle $style)
    {
        if (method_exists($style, 'getBorderChars')) {
            return $this->setPaddingFromSymfony5Style($style);
        }

        $verticalBorderLen = strlen(sprintf($style->getBorderFormat(), $style->getVerticalBorderChar()));
        $paddingLen = strlen($style->getPaddingChar());

        $this->extraPaddingAtBeginningOfLine = 0;
        $this->extraPaddingAtEndOfLine = $verticalBorderLen;
        $this->paddingInEachCell = $verticalBorderLen + $paddingLen + 1;
    }

    /**
     * Calculate our padding widths from the specified table style.
     * @param TableStyle $style
     */
    public function setPaddingFromSymfony5Style(TableStyle $style)
    {
        $borderChars = $style->getBorderChars();
        $verticalBorderChar = $borderChars[1];
        $verticalBorderLen = strlen(sprintf($style->getBorderFormat(), $verticalBorderChar));
        $paddingLen = strlen($style->getPaddingChar());

        $this->extraPaddingAtBeginningOfLine = 0;
        $this->extraPaddingAtEndOfLine = $verticalBorderLen;
        $this->paddingInEachCell = $verticalBorderLen + $paddingLen + 1;
    }

    /**
     * If columns have minimum widths, then set them here.
     * @param array $minimumWidths
     */
    public function setMinimumWidths($minimumWidths)
    {
        $this->minimumWidths = new ColumnWidths($minimumWidths);
    }

    /**
     * Set the minimum width of just one column
     */
    public function minimumWidth($colkey, $width)
    {
        $this->minimumWidths->setWidth($colkey, $width);
    }

    /**
     * Wrap the cells in each part of the provided data table
     * @param array $rows
     * @return array
     */
    public function wrap($rows, $widths = [])
    {
        $auto_widths = $this->calculateWidths($rows, $widths);

        // If no widths were provided, then disable wrapping
        if ($auto_widths->isEmpty()) {
            return $rows;
        }

        // Do wordwrap on all cells.
        $newrows = array();
        foreach ($rows as $rowkey => $row) {
            foreach ($row as $colkey => $cell) {
                $newrows[$rowkey][$colkey] = $this->wrapCell($cell, $auto_widths->width($colkey));
            }
        }

        return $newrows;
    }

    /**
     * Determine what widths we'll use for wrapping.
     */
    protected function calculateWidths($rows, $widths = [])
    {
        // Widths must be provided in some form or another, or we won't wrap.
        if (empty($widths) && !$this->width) {
            return new ColumnWidths();
        }

        // Technically, `$widths`, if provided here, should be used
        // as the exact widths to wrap to. For now we'll just treat
        // these as minimum widths
        $minimumWidths = $this->minimumWidths->combine(new ColumnWidths($widths));

        $calculator = new CalculateWidths();
        $dataCellWidths = $calculator->calculateLongestCell($rows);

        $availableWidth = $this->width - $dataCellWidths->paddingSpace($this->paddingInEachCell, $this->extraPaddingAtEndOfLine, $this->extraPaddingAtBeginningOfLine);

        $this->minimumWidths->adjustMinimumWidths($availableWidth, $dataCellWidths);

        return $calculator->calculate($availableWidth, $dataCellWidths, $minimumWidths);
    }

    /**
     * Wrap one cell.  Guard against modifying non-strings and
     * then call through to wordwrap().
     *
     * @param mixed $cell
     * @param string $cellWidth
     * @return mixed
     */
    protected function wrapCell($cell, $cellWidth)
    {
        if (!is_string($cell)) {
            return $cell;
        }

        if ($cellWidth == 0) {
            // Fourth parameter cannot be true if parameter 2 is 0.
            return wordwrap($cell, $cellWidth, "\n", false);
        }
        return wordwrap($cell, $cellWidth, "\n", true);
    }
}
