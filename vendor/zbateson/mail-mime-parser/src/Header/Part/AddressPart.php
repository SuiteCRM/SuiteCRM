<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MbWrapper\MbWrapper;

/**
 * Holds a single address or name/address pair.
 *
 * The name part of the address may be mime-encoded, but the email address part
 * can't be mime-encoded.  Any whitespace in the email address part is stripped
 * out.
 *
 * A convenience method, getEmail, is provided for clarity -- but getValue
 * returns the email address as well.
 *
 * @author Zaahid Bateson
 */
class AddressPart extends ParameterPart
{
    /**
     * Performs mime-decoding and initializes the address' name and email.
     *
     * The passed $name may be mime-encoded.  $email is stripped of any
     * whitespace.
     *
     */
    public function __construct(MbWrapper $charsetConverter, string $name, string $email)
    {
        parent::__construct(
            $charsetConverter,
            $name,
            ''
        );
        // can't be mime-encoded
        $this->value = $this->convertEncoding($email);
    }

    /**
     * Returns the email address.
     *
     * @return string The email address.
     */
    public function getEmail() : string
    {
        return $this->value;
    }
}
