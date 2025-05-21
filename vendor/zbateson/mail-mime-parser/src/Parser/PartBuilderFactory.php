<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use Psr\Http\Message\StreamInterface;
use ZBateson\MailMimeParser\Message\PartHeaderContainer;
use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxy;

/**
 * Responsible for creating PartBuilder instances.
 *
 * @author Zaahid Bateson
 */
class PartBuilderFactory
{
    /**
     * Constructs a top-level (message) PartBuilder object and returns it
     *
     * @return PartBuilder
     */
    public function newPartBuilder(PartHeaderContainer $headerContainer, StreamInterface $messageStream)
    {
        return new PartBuilder($headerContainer, $messageStream);
    }

    /**
     * Constructs a child PartBuilder object with the passed $parent as its
     * parent, and returns it
     *
     * @return PartBuilder
     */
    public function newChildPartBuilder(PartHeaderContainer $headerContainer, ParserPartProxy $parent)
    {
        return new PartBuilder(
            $headerContainer,
            null,
            $parent
        );
    }
}
