<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

use ZBateson\MailMimeParser\Header\Consumer\ConsumerService;

/**
 * Represents a Content-ID, Message-ID, In-Reply-To or References header.
 *
 * For a multi-id header like In-Reply-To or References, all IDs can be
 * retrieved by calling {@see IdHeader::getIds()}.  Otherwise, to retrieve the
 * first (or only) ID call {@see IdHeader::getValue()}.
 *
 * @author Zaahid Bateson
 */
class IdHeader extends MimeEncodedHeader
{
    /**
     * Returns an IdBaseConsumer.
     *
     * @return Consumer\AbstractConsumer
     */
    protected function getConsumer(ConsumerService $consumerService)
    {
        return $consumerService->getIdBaseConsumer();
    }

    /**
     * Returns the ID. Synonymous to calling getValue().
     *
     * @return string|null The ID
     */
    public function getId() : ?string
    {
        return $this->getValue();
    }

    /**
     * Returns all IDs parsed for a multi-id header like References or
     * In-Reply-To.
     *
     * @return string[] An array of IDs
     */
    public function getIds() : array
    {
        return $this->parts;
    }
}
