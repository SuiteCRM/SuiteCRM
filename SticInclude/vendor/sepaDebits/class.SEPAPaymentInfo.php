<?php
/**
 * This class represents a PmtInf block within a SEPA message.
 *
 * @author     Johannes Feichtner <johannes@web-wack.at>
 * @copyright  http://www.web-wack.at web wack creations
 * @license    http://creativecommons.org/licenses/by-nc/3.0/ CC Attribution-NonCommercial 3.0 license
 * For commercial use please contact sales@web-wack.at
 */

class SEPAPaymentInfo
{
  /**
   * Specifies the means of payment that will be used to move the amount of money. (Tag: <PmtMtd>)
   *
   * Collection of an amount of money from the debtor's bank account by the creditor.
   * The amount of money and dates of collections may vary.
   *
   * @var string
   */
  public const PAYMENT_METHOD = 'DD';

  /**
   * Specifies a pre-agreed service or level of service between the parties,
   * as published in an external service level code list. (Tag: <SvcLvl->Cd>)
   *
   * @var string
   */
  public const SERVICELEVEL_CODE = 'SEPA';

  /**
   * Specifies which party/parties will bear the charges associated with the
   * processing of the payment transaction. (Tag: <ChrgBr>)
   *
   * @var string
   */
  public const CHARGE_BEARER = 'SLEV';

  /**
   * Name of the identification scheme, in a free text form. (Tag: <CdtrSchmeId->Id->PrvtId->Othr->SchmeNm->Prtry>)
   *
   * @var string
   */
  public const PROPRIETARY_NAME = 'SEPA';

  /**
   * Unique identification, as assigned by a sending party, to unambiguously identify the
   * payment information group within the message. (Tag: <PmtInfId>)
   *
   * @var string
   */
  private $paymentInformationIdentification = '';

  /**
   * Identifies whether a single entry per individual transaction or a batch entry for the
   * sum of the amounts of all transactions within the group of a message is requested (Tag: <BtchBookg>)
   *
   * @var string
   */
  private $batchBooking = 'false';

  /**
   * Number of individual transactions contained in the payment information group (Tag: <NbOfTxs>)
   *
   * @var int
   */
  private $numberOfTransactions = 0;

  /**
   * Total of all individual amounts included in the group (Tag: <CtrlSum>)
   *
   * @var float
   */
  private $controlSum = 0.00;

  /**
   * Specifies the local instrument, as published in an external local instrument code list.
   * Only 'CORE', 'COR1', or 'B2B' are allowed. (Tag: <LclInstrm->Cd>)
   *
   * @var string
   */
  private $localInstrumentCode = 'CORE';

  /**
   * Identifies the direct debit sequence, such as first, recurrent, final or one-off. (Tag: <SeqTp>)
   *
   * @var string
   */
  private $sequenceType = 'FRST';

  /**
   * Date and time at which the creditor requests that the amount of money is to be
   * collected from the debtor. (Tag: <ReqdColltnDt>)
   *
   * @var DateTime object
   */
  private $requestedCollectionDate = null;

  /**
   * Party to which an amount of money is due. (Tag: <Cdtr->Nm>)
   *
   * @var string
   */
  private $creditorName = '';

  /**
   * Unambiguous identification of the account of the creditor to which a credit entry will
   * be posted as a result of the payment transaction. (Tag: <CdtrAcct->Id->IBAN>)
   *
   * @var string
   */
  private $creditorAccountIBAN = '';

  /**
   * Financial institution servicing an account for the creditor. (Tag: <CdtrAgt->FinInstnId->BIC>)
   *
   * @var string
   */
  private $creditorAgentBIC = '';

  /**
   * Credit party that signs the mandate. (Tag: <CdtrSchmeId->Id->PrvtId->Othr->Id>)
   *
   * @var string
   */
  private $creditorSchemeIdentification = '';

  /**
   * A container for all transactions within this PaymentInfo.
   *
   * @var SEPADirectDebitTransaction[] array
   */
  private $transactions = [];

  /**
   * Getter for PaymentInformationIdentification (PmtInfId)
   *
   * @return string
   */
  public function getPaymentInformationIdentification()
  {
    return $this->paymentInformationIdentification;
  }

  /**
   * Setter for PaymentInformationIdentification (PmtInfId)
   *
   * @param string $pmtInfId
   * @throws SEPAException
   */
  public function setPaymentInformationIdentification($pmtInfId)
  {
    $pmtInfId = URLify::downcode($pmtInfId, "de");

    if (!preg_match("/^([A-Za-z0-9]|[\+|\?|\/|\-|:|\(|\)|\.|,|'| ]){1,35}\z/", $pmtInfId))
      throw new SEPAException("PmtInfId empty, contains invalid characters or too long (max. 35).");

    $this->paymentInformationIdentification = $pmtInfId;
    
    }

  /**
   * Getter for BatchBooking (BtchBookg)
   *
   * @return string
   */
  public function getBatchBooking()
  {
    return $this->batchBooking;
  }

  /**
   * Setter for BatchBooking (BtchBookg)
   *
   * @param bool|string $btchBookg
   */
  public function setBatchBooking($btchBookg)
  {
    if ($btchBookg === true || $btchBookg == 'true')
      $this->batchBooking = 'true';
    else
      $this->batchBooking = 'false';
    
      }

  /**
   * Getter for Number of Transactions (NbOfTxs)
   *
   * @return int
   */
  public function getNumberOfTransactions()
  {
    return $this->numberOfTransactions;
  }

  /**
   * Getter for ControlSum (CtrlSum)
   *
   * @return float
   */
  public function getControlSum()
  {
    return $this->controlSum;
  }

  /**
   * Getter for LocalInstrument->Code (LclInstrm->Cd)
   *
   * @return string
   */
  public function getLocalInstrumentCode()
  {
    return $this->localInstrumentCode;
  }

  /**
   * Setter for LocalInstrument->Code (LclInstrm->Cd)
   *
   * @param string $cd
   * @throws SEPAException
   */
  public function setLocalInstrumentCode($cd)
  {
    if (!preg_match("/^(B2B|COR1|CORE)\z/", $cd))
      throw new SEPAException("Only 'CORE', 'COR1', or 'B2B' are allowed.");

    $this->localInstrumentCode = $cd;
    
    }

  /**
   * Getter for SequenceType (SeqTp)
   *
   * @return string
   */
  public function getSequenceType()
  {
    return $this->sequenceType;
  }

  /**
   * Setter for SequenceType (SeqTp)
   *
   * @param string $seqTp
   * @throws SEPAException
   */
  public function setSequenceType($seqTp)
  {
    if (!preg_match("/^(FNAL|FRST|OOFF|RCUR)\z/", $seqTp))
      throw new SEPAException("Only 'FNAL', 'FRST', 'OOFF', or 'RCUR' are allowed.");

    $this->sequenceType = $seqTp;
  }

  /**
   * Getter for Requested Collection Date (ReqdColltnDt)
   *
   * @return DateTime object
   */
  public function getRequestedCollectionDate()
  {
    $reqdColltnDt = new DateTime();

    if (!$this->requestedCollectionDate)
      $this->requestedCollectionDate = $reqdColltnDt->format('Y-m-d');

    return $this->requestedCollectionDate;
  }

  /**
   * Setter for Requested Collection Date (ReqdColltnDt)
   *
   * @param string $reqdColltnDt
   */
  public function setRequestedCollectionDate($reqdColltnDt)
  {
    $this->requestedCollectionDate = $reqdColltnDt;
  }

  /**
   * Getter for Creditor Name (Cdtr->Nm)
   *
   * @return string
   */
  public function getCreditorName()
  {
    return $this->creditorName;
  }

  /**
   * Setter for Creditor Name (Cdtr->Nm)
   *
   * @param string $cdtr
   * @throws SEPAException
   */
  public function setCreditorName($cdtr)
  {
    $cdtr = URLify::downcode($cdtr, "de");

    if (strlen($cdtr) == 0 || strlen($cdtr) > 70)
      throw new SEPAException("Invalid creditor name (max. 70 characters).");

    $this->creditorName = $cdtr;
  }

  /**
   * Getter for Creditor Account IBAN (CdtrAcct->Id->IBAN)
   *
   * @return string
   */
  public function getCreditorAccountIBAN()
  {
    return $this->creditorAccountIBAN;
  }

  /**
   * Setter for Creditor Account IBAN (CdtrAcct->Id->IBAN)
   *
   * @param string $iban
   * @throws SEPAException
   */
  public function setCreditorAccountIBAN($iban)
  {
    $iban = str_replace(' ', '', trim($iban));

    if (!preg_match("/^[a-zA-Z]{2}[0-9]{2}[a-zA-Z0-9]{4}[0-9]{7}([a-zA-Z0-9]?){0,16}\z/i", $iban))
      throw new SEPAException("Invalid creditor IBAN.");

    $this->creditorAccountIBAN = $iban;
  }

  /**
   * Getter for Creditor Agent BIC (CdtrAgt->FinInstnId->BIC)
   *
   * @return string
   */
  public function getCreditorAgentBIC()
  {
    return $this->creditorAgentBIC;
  }

  /**
   * Setter for Creditor Agent BIC (CdtrAgt->FinInstnId->BIC)
   *
   * @param string $bic
   * @throws SEPAException
   */
  public function setCreditorAgentBIC($bic)
  {
    $bic = str_replace(' ', '', trim($bic));

    if (!preg_match("/^[0-9a-z]{4}[a-z]{2}[0-9a-z]{2}([0-9a-z]{3})?\z/i", $bic))
      throw new SEPAException("Invalid creditor BIC.");

    $this->creditorAgentBIC = $bic;
  }

  /**
   * Getter for Creditor Scheme Identification (CdtrSchmeId->Id->PrvtId->Othr->Id)
   *
   * @return string
   */
  public function getCreditorSchemeIdentification()
  {
    return $this->creditorSchemeIdentification;
  }

  /**
   * Setter for Creditor Scheme Identification (CdtrSchmeId->Id->PrvtId->Othr->Id)
   *
   * @param string $cdtrSchmeId
   * @throws SEPAException
   */
  public function setCreditorSchemeIdentification($cdtrSchmeId)
  {
    if (empty($cdtrSchmeId) || is_null($cdtrSchmeId))
      throw new SEPAException("Invalid CreditorSchemeIdentification.");

    $this->creditorSchemeIdentification = $cdtrSchmeId;
  }

  /**
   * Adds a transaction object to the list of transactions.
   *
   * @param SEPADirectDebitTransaction $transaction
   */
  public function addTransaction(SEPADirectDebitTransaction $transaction)
  {
    $this->transactions[] = $transaction;

    $this->numberOfTransactions++;
    $this->controlSum += $transaction->getInstructedAmount();
  }

  /**
   * Returns a SimpleXMLElement for the SEPAPaymentInfo object.
   *
   * @return SimpleXMLElement object
   */
  public function getXmlPaymentInfo()
  {
    $xml = new SimpleXMLElement("<PmtInf></PmtInf>");
    $xml->addChild('PmtInfId', $this->getPaymentInformationIdentification());
    $xml->addChild('PmtMtd', self::PAYMENT_METHOD);
    $xml->addChild('BtchBookg',$this->getBatchBooking());
    $xml->addChild('NbOfTxs', $this->getNumberOfTransactions());
    // $xml->addChild('CtrlSum', $this->getControlSum());
    $xml->addChild('CtrlSum', number_format($this->getControlSum(), 2, '.', '')); // SinergiaTIC 10/05/2016

    $xml->addChild('PmtTpInf')->addChild('SvcLvl')->addChild('Cd', self::SERVICELEVEL_CODE);
    $xml->PmtTpInf->addChild('LclInstrm')->addChild('Cd', $this->getLocalInstrumentCode());
    $xml->PmtTpInf->addChild('SeqTp', $this->getSequenceType());

    $xml->addChild('ReqdColltnDt', $this->getRequestedCollectionDate());
    $xml->addChild('Cdtr')->addChild('Nm', $this->getCreditorName());
    $xml->addChild('CdtrAcct')->addChild('Id')->addChild('IBAN', $this->getCreditorAccountIBAN());

    // The BIC is optional for national payments (IBAN only)
    $bic = $this->getCreditorAgentBIC();
    if (!empty($bic))
      $xml->addChild('CdtrAgt')->addChild('FinInstnId')->addChild('BIC', $bic);
    else
      $xml->addChild('CdtrAgt')->addChild('FinInstnId')->addChild('Othr')->addChild('Id', 'NOTPROVIDED');

    $xml->addChild('ChrgBr', self::CHARGE_BEARER);

    $othr = $xml->addChild('CdtrSchmeId')->addChild('Id')->addChild('PrvtId')->addChild('Othr');
    $cdtrSchmeId = $this->getCreditorSchemeIdentification();
    if (!empty($cdtrSchmeId))
      $othr->addChild('Id', $this->getCreditorSchemeIdentification());

    $othr->addChild('SchmeNm')->addChild('Prtry', self::PROPRIETARY_NAME);

    // Add all transactions to the current PaymentInfo block
    for ($i = 0; $i < $this->getNumberOfTransactions(); $i++)
    {
      $domPaymentInfo = dom_import_simplexml($xml);
      $domTransaction = dom_import_simplexml($this->transactions[$i]->getXmlDirectDebitTransaction());
      $domPaymentInfo->appendChild($domPaymentInfo->ownerDocument->importNode($domTransaction, true));
    }
    $xml->NbOfTxs = $this->getNumberOfTransactions();
    // $xml->CtrlSum = $this->getControlSum();
    $xml->CtrlSum = number_format($this->getControlSum(), 2, '.', ''); // SinergiaTIC 10/05/2016

    return $xml;
  }
}
