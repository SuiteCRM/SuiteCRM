<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

use ZBateson\MailMimeParser\Header\Consumer\Received\DomainConsumer;
use ZBateson\MailMimeParser\Header\Consumer\Received\GenericReceivedConsumer;
use ZBateson\MailMimeParser\Header\Consumer\Received\ReceivedDateConsumer;
use ZBateson\MailMimeParser\Header\Part\HeaderPartFactory;
use ZBateson\MailMimeParser\Header\Part\MimeLiteralPartFactory;

/**
 * Simple service provider for consumer singletons.
 *
 * @author Zaahid Bateson
 */
class ConsumerService
{
    /**
     * @var \ZBateson\MailMimeParser\Header\Part\HeaderPartFactory the
     * HeaderPartFactory instance used to create HeaderParts.
     */
    protected $partFactory;

    /**
     * @var \ZBateson\MailMimeParser\Header\Part\MimeLiteralPartFactory used for
     *      GenericConsumer instances.
     */
    protected $mimeLiteralPartFactory;

    /**
     * @var Received\DomainConsumer[]|Received\GenericReceivedConsumer[]|Received\ReceivedDateConsumer[]
     *      an array of sub-received header consumer instances.
     */
    protected $receivedConsumers = [
        'from' => null,
        'by' => null,
        'via' => null,
        'with' => null,
        'id' => null,
        'for' => null,
        'date' => null
    ];

    public function __construct(HeaderPartFactory $partFactory, MimeLiteralPartFactory $mimeLiteralPartFactory)
    {
        $this->partFactory = $partFactory;
        $this->mimeLiteralPartFactory = $mimeLiteralPartFactory;
    }

    /**
     * Returns the AddressBaseConsumer singleton instance.
     *
     * @return AddressBaseConsumer
     */
    public function getAddressBaseConsumer()
    {
        return AddressBaseConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the AddressConsumer singleton instance.
     *
     * @return AddressConsumer
     */
    public function getAddressConsumer()
    {
        return AddressConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the AddressGroupConsumer singleton instance.
     *
     * @return AddressGroupConsumer
     */
    public function getAddressGroupConsumer()
    {
        return AddressGroupConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the AddressEmailConsumer singleton instance.
     *
     * @return AddressEmailConsumer
     */
    public function getAddressEmailConsumer()
    {
        return AddressEmailConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the CommentConsumer singleton instance.
     *
     * @return CommentConsumer
     */
    public function getCommentConsumer()
    {
        return CommentConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the GenericConsumer singleton instance.
     *
     * @return GenericConsumer
     */
    public function getGenericConsumer()
    {
        return GenericConsumer::getInstance($this, $this->mimeLiteralPartFactory);
    }

    /**
     * Returns the SubjectConsumer singleton instance.
     *
     * @return SubjectConsumer
     */
    public function getSubjectConsumer()
    {
        return SubjectConsumer::getInstance($this, $this->mimeLiteralPartFactory);
    }

    /**
     * Returns the QuotedStringConsumer singleton instance.
     *
     * @return QuotedStringConsumer
     */
    public function getQuotedStringConsumer()
    {
        return QuotedStringConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the DateConsumer singleton instance.
     *
     * @return DateConsumer
     */
    public function getDateConsumer()
    {
        return DateConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the ParameterConsumer singleton instance.
     *
     * @return ParameterConsumer
     */
    public function getParameterConsumer()
    {
        return ParameterConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the consumer instance corresponding to the passed part name of a
     * Received header.
     *
     * @return AbstractConsumer
     */
    public function getSubReceivedConsumer(string $partName)
    {
        if (empty($this->receivedConsumers[$partName])) {
            $consumer = null;
            if ($partName === 'from' || $partName === 'by') {
                $consumer = new DomainConsumer($this, $this->partFactory, $partName);
            } elseif ($partName === 'date') {
                $consumer = new ReceivedDateConsumer($this, $this->partFactory);
            } else {
                $consumer = new GenericReceivedConsumer($this, $this->partFactory, $partName);
            }
            $this->receivedConsumers[$partName] = $consumer;
        }
        return $this->receivedConsumers[$partName];
    }

    /**
     * Returns the ReceivedConsumer singleton instance.
     *
     * @return ReceivedConsumer
     */
    public function getReceivedConsumer()
    {
        return ReceivedConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the IdConsumer singleton instance.
     *
     * @return IdConsumer
     */
    public function getIdConsumer()
    {
        return IdConsumer::getInstance($this, $this->partFactory);
    }

    /**
     * Returns the IdBaseConsumer singleton instance.
     *
     * @return IdBaseConsumer
     */
    public function getIdBaseConsumer()
    {
        return IdBaseConsumer::getInstance($this, $this->partFactory);
    }
}
