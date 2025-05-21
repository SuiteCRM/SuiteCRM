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

class RotatingBarcodeValues extends \Google\Collection
{
  protected $collection_key = 'values';
  /**
   * @var string
   */
  public $periodMillis;
  /**
   * @var string
   */
  public $startDateTime;
  /**
   * @var string[]
   */
  public $values;

  /**
   * @param string
   */
  public function setPeriodMillis($periodMillis)
  {
    $this->periodMillis = $periodMillis;
  }
  /**
   * @return string
   */
  public function getPeriodMillis()
  {
    return $this->periodMillis;
  }
  /**
   * @param string
   */
  public function setStartDateTime($startDateTime)
  {
    $this->startDateTime = $startDateTime;
  }
  /**
   * @return string
   */
  public function getStartDateTime()
  {
    return $this->startDateTime;
  }
  /**
   * @param string[]
   */
  public function setValues($values)
  {
    $this->values = $values;
  }
  /**
   * @return string[]
   */
  public function getValues()
  {
    return $this->values;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RotatingBarcodeValues::class, 'Google_Service_Walletobjects_RotatingBarcodeValues');
