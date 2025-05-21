<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MbWrapper\MbWrapper;

/**
 * Represents a mime header comment -- text in a structured mime header
 * value existing within parentheses.
 *
 * @author Zaahid Bateson
 */
class CommentPart extends MimeLiteralPart
{
    /**
     * @var string the contents of the comment
     */
    protected $comment;

    /**
     * Constructs a MimeLiteralPart, decoding the value if it's mime-encoded.
     *
     */
    public function __construct(MbWrapper $charsetConverter, string $token)
    {
        parent::__construct($charsetConverter, $token);
        $this->comment = $this->value;
        $this->value = '';
        $this->canIgnoreSpacesBefore = true;
        $this->canIgnoreSpacesAfter = true;
    }

    /**
     * Returns the comment's text.
     *
     */
    public function getComment() : string
    {
        return $this->comment;
    }
}
