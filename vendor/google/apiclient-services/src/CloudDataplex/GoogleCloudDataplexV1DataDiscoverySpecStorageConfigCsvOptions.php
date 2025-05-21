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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $delimiter;
  /**
   * @var string
   */
  public $encoding;
  /**
   * @var int
   */
  public $headerRows;
  /**
   * @var string
   */
  public $quote;
  /**
   * @var bool
   */
  public $typeInferenceDisabled;

  /**
   * @param string
   */
  public function setDelimiter($delimiter)
  {
    $this->delimiter = $delimiter;
  }
  /**
   * @return string
   */
  public function getDelimiter()
  {
    return $this->delimiter;
  }
  /**
   * @param string
   */
  public function setEncoding($encoding)
  {
    $this->encoding = $encoding;
  }
  /**
   * @return string
   */
  public function getEncoding()
  {
    return $this->encoding;
  }
  /**
   * @param int
   */
  public function setHeaderRows($headerRows)
  {
    $this->headerRows = $headerRows;
  }
  /**
   * @return int
   */
  public function getHeaderRows()
  {
    return $this->headerRows;
  }
  /**
   * @param string
   */
  public function setQuote($quote)
  {
    $this->quote = $quote;
  }
  /**
   * @return string
   */
  public function getQuote()
  {
    return $this->quote;
  }
  /**
   * @param bool
   */
  public function setTypeInferenceDisabled($typeInferenceDisabled)
  {
    $this->typeInferenceDisabled = $typeInferenceDisabled;
  }
  /**
   * @return bool
   */
  public function getTypeInferenceDisabled()
  {
    return $this->typeInferenceDisabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataDiscoverySpecStorageConfigCsvOptions');
