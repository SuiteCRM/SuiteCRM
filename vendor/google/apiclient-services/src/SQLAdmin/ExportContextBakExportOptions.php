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

namespace Google\Service\SQLAdmin;

class ExportContextBakExportOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $bakType;
  /**
   * @var bool
   */
  public $copyOnly;
  /**
   * @var bool
   */
  public $differentialBase;
  /**
   * @var string
   */
  public $exportLogEndTime;
  /**
   * @var string
   */
  public $exportLogStartTime;
  /**
   * @var int
   */
  public $stripeCount;
  /**
   * @var bool
   */
  public $striped;

  /**
   * @param string
   */
  public function setBakType($bakType)
  {
    $this->bakType = $bakType;
  }
  /**
   * @return string
   */
  public function getBakType()
  {
    return $this->bakType;
  }
  /**
   * @param bool
   */
  public function setCopyOnly($copyOnly)
  {
    $this->copyOnly = $copyOnly;
  }
  /**
   * @return bool
   */
  public function getCopyOnly()
  {
    return $this->copyOnly;
  }
  /**
   * @param bool
   */
  public function setDifferentialBase($differentialBase)
  {
    $this->differentialBase = $differentialBase;
  }
  /**
   * @return bool
   */
  public function getDifferentialBase()
  {
    return $this->differentialBase;
  }
  /**
   * @param string
   */
  public function setExportLogEndTime($exportLogEndTime)
  {
    $this->exportLogEndTime = $exportLogEndTime;
  }
  /**
   * @return string
   */
  public function getExportLogEndTime()
  {
    return $this->exportLogEndTime;
  }
  /**
   * @param string
   */
  public function setExportLogStartTime($exportLogStartTime)
  {
    $this->exportLogStartTime = $exportLogStartTime;
  }
  /**
   * @return string
   */
  public function getExportLogStartTime()
  {
    return $this->exportLogStartTime;
  }
  /**
   * @param int
   */
  public function setStripeCount($stripeCount)
  {
    $this->stripeCount = $stripeCount;
  }
  /**
   * @return int
   */
  public function getStripeCount()
  {
    return $this->stripeCount;
  }
  /**
   * @param bool
   */
  public function setStriped($striped)
  {
    $this->striped = $striped;
  }
  /**
   * @return bool
   */
  public function getStriped()
  {
    return $this->striped;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExportContextBakExportOptions::class, 'Google_Service_SQLAdmin_ExportContextBakExportOptions');
