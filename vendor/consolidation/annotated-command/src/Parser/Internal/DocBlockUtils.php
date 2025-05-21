<?php
namespace Consolidation\AnnotatedCommand\Parser\Internal;

/**
 * Simple utility methods when working with docblock comments.
 */
class DocBlockUtils
{
    public static function stripLeadingCommentCharacters($doc)
    {
        // Remove the leading /** and the trailing */
        $doc = preg_replace('#^\s*/\*+\s*#', '', $doc);
        $doc = preg_replace('#\s*\*+/\s*#', '', $doc);
        $doc = preg_replace('#^[ \t]*\** ?#m', '', $doc);

        return $doc;
    }

    public static function nextLineIsNotEmpty($lines)
    {
        if (empty($lines)) {
            return false;
        }

        $nextLine = trim($lines[0]);
        return !empty($nextLine);
    }
}
