<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Walletobjects;

class RotatingBarcode extends \Google\Model
{
  /**
   * @var string
   */
  public $alternateText;
  protected $initialRotatingBarcodeValuesType = RotatingBarcodeValues::class;
  protected $initialRotatingBarcodeValuesDataType = '';
  /**
   * @var string
   */
  public $renderEncoding;
  protected $showCodeTextType = LocalizedString::class;
  protected $showCodeTextDataType = '';
  protected $totpDetailsType = RotatingBarcodeTotpDetails::class;
  protected $totpDetailsDataType = '';
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $valuePattern;

  /**
   * @param string
   */
  public function setAlternateText($alternateText)
  {
    $this->alternateText = $alternateText;
  }
  /**
   * @return string
   */
  public function getAlternateText()
  {
    return $this->alternateText;
  }
  /**
   * @param RotatingBarcodeValues
   */
  public function setInitialRotatingBarcodeValues(RotatingBarcodeValues $initialRotatingBarcodeValues)
  {
    $this->initialRotatingBarcodeValues = $initialRotatingBarcodeValues;
  }
  /**
   * @return RotatingBarcodeValues
   */
  public function getInitialRotatingBarcodeValues()
  {
    return $this->initialRotatingBarcodeValues;
  }
  /**
   * @param string
   */
  public function setRenderEncoding($renderEncoding)
  {
    $this->renderEncoding = $renderEncoding;
  }
  /**
   * @return string
   */
  public function getRenderEncoding()
  {
    return $this->renderEncoding;
  }
  /**
   * @param LocalizedString
   */
  public function setShowCodeText(LocalizedString $showCodeText)
  {
    $this->showCodeText = $showCodeText;
  }
  /**
   * @return LocalizedString
   */
  public function getShowCodeText()
  {
    return $this->showCodeText;
  }
  /**
   * @param RotatingBarcodeTotpDetails
   */
  public function setTotpDetails(RotatingBarcodeTotpDetails $totpDetails)
  {
    $this->totpDetails = $totpDetails;
  }
  /**
   * @return RotatingBarcodeTotpDetails
   */
  public function getTotpDetails()
  {
    return $this->totpDetails;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setValuePattern($valuePattern)
  {
    $this->valuePattern = $valuePattern;
  }
  /**
   * @return string
   */
  public function getValuePattern()
  {
    return $this->valuePattern;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RotatingBarcode::class, 'Google_Service_Walletobjects_RotatingBarcode');
