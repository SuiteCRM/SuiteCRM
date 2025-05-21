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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1FeatureViewSyncSyncSummary extends \Google\Model
{
  /**
   * @var string
   */
  public $rowSynced;
  /**
   * @var string
   */
  public $systemWatermarkTime;
  /**
   * @var string
   */
  public $totalSlot;

  /**
   * @param string
   */
  public function setRowSynced($rowSynced)
  {
    $this->rowSynced = $rowSynced;
  }
  /**
   * @return string
   */
  public function getRowSynced()
  {
    return $this->rowSynced;
  }
  /**
   * @param string
   */
  public function setSystemWatermarkTime($systemWatermarkTime)
  {
    $this->systemWatermarkTime = $systemWatermarkTime;
  }
  /**
   * @return string
   */
  public function getSystemWatermarkTime()
  {
    return $this->systemWatermarkTime;
  }
  /**
   * @param string
   */
  public function setTotalSlot($totalSlot)
  {
    $this->totalSlot = $totalSlot;
  }
  /**
   * @return string
   */
  public function getTotalSlot()
  {
    return $this->totalSlot;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureViewSyncSyncSummary::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureViewSyncSyncSummary');
