<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Part;

use ZBateson\MailMimeParser\Parser\Proxy\ParserMimePartProxy;

/**
 * Creates ParserPartChildrenContainer instances.
 *
 * @author Zaahid Bateson
 */
class ParserPartChildrenContainerFactory
{
    public function newInstance(ParserMimePartProxy $parserProxy)
    {
        return new ParserPartChildrenContainer($parserProxy);
    }
}
