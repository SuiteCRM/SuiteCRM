<?php
/**
 * This class represents a single transaction.
 *
 * @author     Johannes Feichtner <johannes@web-wack.at>
 * @copyright  http://www.web-wack.at web wack creations
 * @license    http://creativecommons.org/licenses/by-nc/3.0/ CC Attribution-NonCommercial 3.0 license
 * For commercial use please contact sales@web-wack.at
 */

class SEPADirectDebitTransaction {
  /**
   * Code allocated to a currency, by a maintenance agency, under an international identification
   * scheme as described in the latest edition of the international standard ISO 4217.
   */
  public const CURRENCY_CODE = 'EUR';

  /**
   * The instruction identification is a point to point reference that can be used between
   * the instructing party and the instructed party to refer to the individual instruction.
   * It can be included in several messages related to the instruction. (Tag: <DrctDbtTxInf->PmtId->InstrId>)
   *
   * @var string
   */
  private $instructionIdentification = '';

  /**
   * The end-to-end identification can be used for reconciliation or to link tasks relating to the transaction.
   * It can be included in several messages related to the transaction. (Tag: <DrctDbtTxInf->PmtId->EndToEndId>)
   *
   * @var string
   */
  private $endToEndIdentification = '';

  /**
   * Amount of money to be moved between the debtor and creditor, before deduction of charges,
   * expressed in the currency as ordered by the initiating party. (Tag: <DrctDbtTxInf->InstdAmt>)
   *
   * @var float
   */
  private $instructedAmount = 0.00;

  /**
   * Unique identification, as assigned by the creditor,
   * to unambiguously identify the mandate. (Tag: <DrctDbtTxInf->DrctDbtTx->MndtRltdInf->MndtId>)
   *
   * @var string
   */
  private $mandateIdentification = '';

  /**
   * Date on which the direct debit mandate has been signed
   * by the debtor. (Tag: <DrctDbtTxInf->DrctDbtTx->MndtRltdInf->DtOfSgntr>)
   *
   * @var string
   */
  private $dateOfSignature = '';

  /**
   * Indicator notifying whether the underlying mandate is amended or not.
   * (Tag: <DrctDbtTxInf->DrctDbtTx->MndtRltdInf->AmdmntInd>)
   *
   * @var string
   */
  private $amendmentIndicator = 'false';

  /**
   * Financial institution servicing an account for the debtor. (Tag: <DbtrAgt->FinInstnId->BIC>)
   *
   * @var string
   */
  private $debtorAgentBIC = '';

  /**
   * Party that owes an amount of money to the (ultimate) creditor. (Tag: <Dbtr->Nm>)
   *
   * @var string
   */
  private $debtorName = '';

  /**
   * Unambiguous identification of the account of the debtor to which a debit entry will
   * be made as a result of the transaction. (Tag: <DbtrAcct->Id->IBAN>)
   *
   * @var string
   */
  private $debtorIBAN = '';

  /**
   * Information supplied to enable the matching/reconciliation of an entry with the items that the
   * payment is intended to settle, such as commercial invoices in an accounts' receivable system.
   * (Tag: <RmtInf->Ustrd>)
   *
   * @var string
   */
  private $remittanceInformation = '';

  /**
   * Getter for Instruction Identification (DrctDbtTxInf->PmtId->InstrId)
   *
   * @return string
   */
  public function getInstructionIdentification()
  {
    if (empty($this->instructionIdentification))
      return $this->getEndToEndIdentification();

    return $this->instructionIdentification;
  }

  /**
   * Setter for Instruction Identification (DrctDbtTxInf->PmtId->InstrId)
   *
   * @param string $instrId
   * @throws SEPAException
   */
  public function setInstructionIdentification($instrId)
  {
    $instrId = URLify::downcode($instrId, "de");

    if (!preg_match("/^([\-A-Za-z0-9\+\/\?:\(\)\., ]){0,35}\z/", $instrId))
      throw new SEPAException("Invalid InstructionIdentification (max. 35).");

    $this->instructionIdentification = $instrId;
  }

  /**
   * Getter for EndToEnd Identification (DrctDbtTxInf->PmtId->EndToEndId)
   *
   * @return string
   */
  public function getEndToEndIdentification()
  {
    return $this->endToEndIdentification;
  }

  /**
   * Setter for EndToEnd Identification (DrctDbtTxInf->PmtId->EndToEndId)
   *
   * @param string $endToEndId
   * @throws SEPAException
   */
  public function setEndToEndIdentification($endToEndId)
  {
    $endToEndId = URLify::downcode($endToEndId, "de");

    if (!preg_match("/^([\-A-Za-z0-9\+\/\?:\(\)\., ]){1,35}\z/", $endToEndId))
      throw new SEPAException("Invalid EndToEndIdentification (max. 35).");

    $this->endToEndIdentification = $endToEndId;
  }

  /**
   * Getter for Instructed Amount (DrctDbtTxInf->InstdAmt)
   *
   * @return float
   */
  public function getInstructedAmount()
  {
    return $this->instructedAmount;
  }

  /**
   * Setter for Instructed Amount (DrctDbtTxInf->InstdAmt)
   *
   * @param float $instdAmt
   */
  public function setInstructedAmount($instdAmt)
  {
    $this->instructedAmount = number_format(floatval($instdAmt), 2, '.', '');
  }

  /**
   * Getter for Mandate Identification (DrctDbtTxInf->DrctDbtTx->MndtRltdInf->MndtId)
   *
   * @return string
   */
  public function getMandateIdentification()
  {
    return $this->mandateIdentification;
  }

  /**
   * Setter for Mandate Identification (DrctDbtTxInf->DrctDbtTx->MndtRltdInf->MndtId)
   *
   * @param string $mndtId
   * @throws SEPAException
   */
  public function setMandateIdentification($mndtId)
  {
    $mndtId = URLify::downcode($mndtId, "de");

    if (!preg_match("/^([A-Za-z0-9]|[\+|\?|\/|\-|:|\(|\)|\.|,|']){1,35}\z/", $mndtId))
      throw new SEPAException("MndtId empty, contains invalid characters or too long (max. 35).");

    $this->mandateIdentification = $mndtId;
  }

  /**
   * Getter for Mandate Identification (DrctDbtTxInf->DrctDbtTx->MndtRltdInf->DtOfSgntr)
   *
   * @return string
   */
  public function getDateOfSignature()
  {
    return $this->dateOfSignature;
  }

  /**
   * Setter for Mandate Identification (DrctDbtTxInf->DrctDbtTx->MndtRltdInf->DtOfSgntr)
   *
   * @param string $dtOfSgntr
   */
  public function setDateOfSignature($dtOfSgntr)
  {
    $this->dateOfSignature = $dtOfSgntr;
  }

  /**
   * Getter for Amendment Indicator (DrctDbtTxInf->DrctDbtTx->MndtRltdInf->AmdmntInd)
   *
   * @return string
   */
  public function getAmendmentIndicator()
  {
    return $this->amendmentIndicator;
  }

  /**
   * Setter for Amendment Indicator (DrctDbtTxInf->DrctDbtTx->MndtRltdInf->AmdmntInd)
   *
   * @param string $amdmntInd
   */
  public function setAmendmentIndicator($amdmntInd)
  {
    if ($amdmntInd === true || $amdmntInd == 'true')
      $this->amendmentIndicator = 'true';
    else
      $this->amendmentIndicator = 'false';
  }

  /**
   * Getter for Debtor Agent BIC (DbtrAgt->FinInstnId->BIC)
   *
   * @return string
   */
  public function getDebtorAgentBIC()
  {
    return $this->debtorAgentBIC;
  }

  /**
   * Setter for Debtor Agent BIC (DbtrAgt->FinInstnId->BIC)
   *
   * @param string $bic
   * @throws SEPAException
   */
  public function setDebtorAgentBIC($bic)
  {
    $bic = str_replace(' ', '', trim($bic));

    if (!preg_match("/^[0-9a-z]{4}[a-z]{2}[0-9a-z]{2}([0-9a-z]{3})?\z/i", $bic))
      throw new SEPAException("Invalid debtor BIC.");

    $this->debtorAgentBIC = $bic;
  }

  /**
   * Getter for Debtor Name (Dbtr->Nm)
   *
   * @return string
   */
  public function getDebtorName()
  {
    return $this->debtorName;
  }

  /**
   * Setter for Debtor Name (Dbtr->Nm)
   *
   * @param string $dbtr
   * @throws SEPAException
   */
  public function setDebtorName($dbtr)
  {
    $dbtr = URLify::downcode($dbtr, "de");

    if (strlen($dbtr) == 0 || strlen($dbtr) > 70)
      throw new SEPAException("Invalid debtor name (max. 70 characters).");

    $this->debtorName = $dbtr;
  }

  /**
   * Getter for Debtor IBAN (DbtrAcct->Id->IBAN)
   *
   * @return string
   */
  public function getDebtorIBAN()
  {
    return $this->debtorIBAN;
  }

  /**
   * Setter for Debtor IBAN (DbtrAcct->Id->IBAN)
   *
   * @param string $iban
   * @throws SEPAException
   */
  public function setDebtorIBAN($iban)
  {
    $iban = str_replace(' ', '', trim($iban));
    //20180608 jch omitimos la comprobación de expresión regular del IBAN para evitar que genere el fichero en blanco al encontrar error
    // if (!preg_match("/^[a-zA-Z]{2}[0-9]{2}[a-zA-Z0-9]{4}[0-9]{7}([a-zA-Z0-9]?){0,16}\z/i", $iban))
    //   throw new SEPAException("Invalid debtor IBAN.");

    $this->debtorIBAN = $iban;
  }

  /**
   * Getter for Remittance Information (RmtInf->Ustrd)
   *
   * @return string
   */
  public function getRemittanceInformation()
  {
    return $this->remittanceInformation;
  }

  /**
   * Getter for Remittance Information (RmtInf->Ustrd)
   *
   * @param string $ustrd
   * @throws SEPAException
   */
  public function setRemittanceInformation($ustrd)
  {
    $ustrd = URLify::downcode($ustrd, "de");

    if (!preg_match("/^([A-Za-z0-9]|[\+|\?|\/|\-|:|\(|\)|\.|,|'| ]){0,140}\z/", $ustrd))
      throw new SEPAException("RmtInf contains invalid chars or is too long (max. 140).");

    $this->remittanceInformation = $ustrd;
  }

  /**
   * Returns a SimpleXMLElement for the SEPADirectDebitTransaction object.
   *
   * @return SimpleXMLElement object
   */
  public function getXmlDirectDebitTransaction()
  {
    $xml = new SimpleXMLElement("<DrctDbtTxInf></DrctDbtTxInf>");
    $xml->addChild('PmtId')->addChild('InstrId', $this->getInstructionIdentification());

    $endToEndId = $this->getEndToEndIdentification();
    if (empty($endToEndId))
      $endToEndId = 'NOTPROVIDED';
    $xml->PmtId->addChild('EndToEndId', $endToEndId);

    $xml->addChild('InstdAmt', $this->getInstructedAmount())->addAttribute('Ccy', self::CURRENCY_CODE);

    $xml->addChild('DrctDbtTx')->addChild('MndtRltdInf')->addChild('MndtId', $this->getMandateIdentification());
    $xml->DrctDbtTx->MndtRltdInf->addChild('DtOfSgntr', $this->getDateOfSignature());
    $xml->DrctDbtTx->MndtRltdInf->addChild('AmdmntInd', $this->getAmendmentIndicator());

    // The BIC is optional for national payments (IBAN only)
    $bic = $this->getDebtorAgentBIC();
    if (!empty($bic))
      $xml->addChild('DbtrAgt')->addChild('FinInstnId')->addChild('BIC', $bic);
    else
      $xml->addChild('DbtrAgt')->addChild('FinInstnId')->addChild('Othr')->addChild('Id', 'NOTPROVIDED');

    $xml->addChild('Dbtr')->addChild('Nm', $this->getDebtorName());
    $xml->addChild('DbtrAcct')->addChild('Id')->addChild('IBAN', $this->getDebtorIBAN());

    $xml->addChild('RmtInf')->addChild('Ustrd', $this->getRemittanceInformation());

    return $xml;
  }
}
