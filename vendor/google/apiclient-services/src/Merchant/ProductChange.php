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

namespace Google\Service\Merchant;

class ProductChange extends \Google\Model
{
  /**
   * @var string
   */
  public $newValue;
  /**
   * @var string
   */
  public $oldValue;
  /**
   * @var string
   */
  public $regionCode;
  /**
   * @var string
   */
  public $reportingContext;

  /**
   * @param string
   */
  public function setNewValue($newValue)
  {
    $this->newValue = $newValue;
  }
  /**
   * @return string
   */
  public function getNewValue()
  {
    return $this->newValue;
  }
  /**
   * @param string
   */
  public function setOldValue($oldValue)
  {
    $this->oldValue = $oldValue;
  }
  /**
   * @return string
   */
  public function getOldValue()
  {
    return $this->oldValue;
  }
  /**
   * @param string
   */
  public function setRegionCode($regionCode)
  {
    $this->regionCode = $regionCode;
  }
  /**
   * @return string
   */
  public function getRegionCode()
  {
    return $this->regionCode;
  }
  /**
   * @param string
   */
  public function setReportingContext($reportingContext)
  {
    $this->reportingContext = $reportingContext;
  }
  /**
   * @return string
   */
  public function getReportingContext()
  {
    return $this->reportingContext;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProductChange::class, 'Google_Service_Merchant_ProductChange');
