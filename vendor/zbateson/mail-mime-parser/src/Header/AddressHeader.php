<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

use ZBateson\MailMimeParser\Header\Consumer\AbstractConsumer;
use ZBateson\MailMimeParser\Header\Consumer\ConsumerService;
use ZBateson\MailMimeParser\Header\Part\AddressGroupPart;
use ZBateson\MailMimeParser\Header\Part\AddressPart;

/**
 * A header containing one or more email addresses and/or groups of addresses.
 *
 * An address is separated by a comma, and each group separated by a semi-colon.
 * The AddressHeader provides a complete list of all addresses referenced in a
 * header including any addresses in groups, in addition to being able to access
 * the groups separately if needed.
 *
 * For full specifications, see {@link https://www.ietf.org/rfc/rfc2822.txt}
 *
 * @author Zaahid Bateson
 */
class AddressHeader extends AbstractHeader
{
    /**
     * @var AddressPart[] array of addresses, included all addresses contained
     *      in groups.
     */
    protected $addresses = [];

    /**
     * @var AddressGroupPart[] array of address groups (lists).
     */
    protected $groups = [];

    /**
     * Returns an AddressBaseConsumer.
     *
     * @return \ZBateson\MailMimeParser\Header\Consumer\AbstractConsumer
     */
    protected function getConsumer(ConsumerService $consumerService)
    {
        return $consumerService->getAddressBaseConsumer();
    }

    /**
     * Overridden to extract all addresses into addresses array.
     *
     * @return static
     */
    protected function setParseHeaderValue(AbstractConsumer $consumer)
    {
        parent::setParseHeaderValue($consumer);
        foreach ($this->parts as $part) {
            if ($part instanceof AddressPart) {
                $this->addresses[] = $part;
            } elseif ($part instanceof AddressGroupPart) {
                $this->addresses = \array_merge($this->addresses, $part->getAddresses());
                $this->groups[] = $part;
            }
        }
        return $this;
    }

    /**
     * Returns all address parts in the header including any addresses that are
     * in groups (lists).
     *
     * @return AddressPart[] The addresses.
     */
    public function getAddresses() : array
    {
        return $this->addresses;
    }

    /**
     * Returns all group parts (lists) in the header.
     *
     * @return AddressGroupPart[]
     */
    public function getGroups() : array
    {
        return $this->groups;
    }

    /**
     * Returns true if an address exists with the passed email address.
     *
     * Comparison is done case insensitively.
     *
     */
    public function hasAddress(string $email) : bool
    {
        foreach ($this->addresses as $addr) {
            if (\strcasecmp($addr->getEmail(), $email) === 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the first email address in the header.
     *
     * @return ?string The email address
     */
    public function getEmail() : ?string
    {
        return $this->getValue();
    }

    /**
     * Returns the name associated with the first email address to complement
     * getValue()/getEmail() if one is set, or null if not.
     *
     * @return string|null The person name.
     */
    public function getPersonName() : ?string
    {
        if (!empty($this->parts)) {
            return $this->parts[0]->getName();
        }
        return null;
    }
}
