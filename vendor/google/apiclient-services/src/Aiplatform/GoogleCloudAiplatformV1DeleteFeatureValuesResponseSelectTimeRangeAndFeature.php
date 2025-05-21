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

class GoogleCloudAiplatformV1DeleteFeatureValuesResponseSelectTimeRangeAndFeature extends \Google\Model
{
  /**
   * @var string
   */
  public $impactedFeatureCount;
  /**
   * @var string
   */
  public $offlineStorageModifiedEntityRowCount;
  /**
   * @var string
   */
  public $onlineStorageModifiedEntityCount;

  /**
   * @param string
   */
  public function setImpactedFeatureCount($impactedFeatureCount)
  {
    $this->impactedFeatureCount = $impactedFeatureCount;
  }
  /**
   * @return string
   */
  public function getImpactedFeatureCount()
  {
    return $this->impactedFeatureCount;
  }
  /**
   * @param string
   */
  public function setOfflineStorageModifiedEntityRowCount($offlineStorageModifiedEntityRowCount)
  {
    $this->offlineStorageModifiedEntityRowCount = $offlineStorageModifiedEntityRowCount;
  }
  /**
   * @return string
   */
  public function getOfflineStorageModifiedEntityRowCount()
  {
    return $this->offlineStorageModifiedEntityRowCount;
  }
  /**
   * @param string
   */
  public function setOnlineStorageModifiedEntityCount($onlineStorageModifiedEntityCount)
  {
    $this->onlineStorageModifiedEntityCount = $onlineStorageModifiedEntityCount;
  }
  /**
   * @return string
   */
  public function getOnlineStorageModifiedEntityCount()
  {
    return $this->onlineStorageModifiedEntityCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1DeleteFeatureValuesResponseSelectTimeRangeAndFeature::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1DeleteFeatureValuesResponseSelectTimeRangeAndFeature');
