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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2CloudDlpInspection extends \Google\Model
{
  /**
   * @var bool
   */
  public $fullScan;
  /**
   * @var string
   */
  public $infoType;
  /**
   * @var string
   */
  public $infoTypeCount;
  /**
   * @var string
   */
  public $inspectJob;

  /**
   * @param bool
   */
  public function setFullScan($fullScan)
  {
    $this->fullScan = $fullScan;
  }
  /**
   * @return bool
   */
  public function getFullScan()
  {
    return $this->fullScan;
  }
  /**
   * @param string
   */
  public function setInfoType($infoType)
  {
    $this->infoType = $infoType;
  }
  /**
   * @return string
   */
  public function getInfoType()
  {
    return $this->infoType;
  }
  /**
   * @param string
   */
  public function setInfoTypeCount($infoTypeCount)
  {
    $this->infoTypeCount = $infoTypeCount;
  }
  /**
   * @return string
   */
  public function getInfoTypeCount()
  {
    return $this->infoTypeCount;
  }
  /**
   * @param string
   */
  public function setInspectJob($inspectJob)
  {
    $this->inspectJob = $inspectJob;
  }
  /**
   * @return string
   */
  public function getInspectJob()
  {
    return $this->inspectJob;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2CloudDlpInspection::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2CloudDlpInspection');
