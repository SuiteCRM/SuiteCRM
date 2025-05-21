<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

use ZBateson\MailMimeParser\Header\Consumer\ConsumerService;

/**
 * Reads a generic header.
 *
 * Header's may contain mime-encoded parts, quoted parts, and comments.  The
 * parsed value is parsed into a single IHeaderPart.
 *
 * @author Zaahid Bateson
 */
class GenericHeader extends AbstractHeader
{
    /**
     * Returns a GenericConsumer.
     *
     * @return Consumer\AbstractConsumer
     */
    protected function getConsumer(ConsumerService $consumerService)
    {
        return $consumerService->getGenericConsumer();
    }
}
