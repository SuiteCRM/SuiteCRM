<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

/**
 * List of header name constants.
 *
 * @author Thomas Landauer
 */
abstract class HeaderConsts
{
    // Headers according to the table at https://tools.ietf.org/html/rfc5322#section-3.6
    public const RETURN_PATH = 'Return-Path';

    public const RECEIVED = 'Received';

    public const RESENT_DATE = 'Resent-Date';

    public const RESENT_FROM = 'Resent-From';

    public const RESENT_SENDER = 'Resent-Sender';

    public const RESENT_TO = 'Resent-To';

    public const RESENT_CC = 'Resent-Cc';

    public const RESENT_BCC = 'Resent-Bcc';

    public const RESENT_MSD_ID = 'Resent-Message-ID';

    public const RESENT_MESSAGE_ID = self::RESENT_MSD_ID;

    public const ORIG_DATE = 'Date';

    public const DATE = self::ORIG_DATE;

    public const FROM = 'From';

    public const SENDER = 'Sender';

    public const REPLY_TO = 'Reply-To';

    public const TO = 'To';

    public const CC = 'Cc';

    public const BCC = 'Bcc';

    public const MESSAGE_ID = 'Message-ID';

    public const IN_REPLY_TO = 'In-Reply-To';

    public const REFERENCES = 'References';

    public const SUBJECT = 'Subject';

    public const COMMENTS = 'Comments';

    public const KEYWORDS = 'Keywords';

    // https://datatracker.ietf.org/doc/html/rfc4021#section-2.2
    public const MIME_VERSION = 'MIME-Version';

    public const CONTENT_TYPE = 'Content-Type';

    public const CONTENT_TRANSFER_ENCODING = 'Content-Transfer-Encoding';

    public const CONTENT_ID = 'Content-ID';

    public const CONTENT_DESCRIPTION = 'Content-Description';

    public const CONTENT_DISPOSITION = 'Content-Disposition';

    public const CONTENT_LANGUAGE = 'Content-Language';

    public const CONTENT_BASE = 'Content-Base';

    public const CONTENT_LOCATION = 'Content-Location';

    public const CONTENT_FEATURES = 'Content-features';

    public const CONTENT_ALTERNATIVE = 'Content-Alternative';

    public const CONTENT_MD5 = 'Content-MD5';

    public const CONTENT_DURATION = 'Content-Duration';

    // https://datatracker.ietf.org/doc/html/rfc3834
    public const AUTO_SUBMITTED = 'Auto-Submitted';
}
