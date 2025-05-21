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

class GoogleCloudAiplatformV1FeatureOnlineStore extends \Google\Model
{
  protected $bigtableType = GoogleCloudAiplatformV1FeatureOnlineStoreBigtable::class;
  protected $bigtableDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dedicatedServingEndpointType = GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint::class;
  protected $dedicatedServingEndpointDataType = '';
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $optimizedType = GoogleCloudAiplatformV1FeatureOnlineStoreOptimized::class;
  protected $optimizedDataType = '';
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudAiplatformV1FeatureOnlineStoreBigtable
   */
  public function setBigtable(GoogleCloudAiplatformV1FeatureOnlineStoreBigtable $bigtable)
  {
    $this->bigtable = $bigtable;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureOnlineStoreBigtable
   */
  public function getBigtable()
  {
    return $this->bigtable;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint
   */
  public function setDedicatedServingEndpoint(GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint $dedicatedServingEndpoint)
  {
    $this->dedicatedServingEndpoint = $dedicatedServingEndpoint;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint
   */
  public function getDedicatedServingEndpoint()
  {
    return $this->dedicatedServingEndpoint;
  }
  /**
   * @param GoogleCloudAiplatformV1EncryptionSpec
   */
  public function setEncryptionSpec(GoogleCloudAiplatformV1EncryptionSpec $encryptionSpec)
  {
    $this->encryptionSpec = $encryptionSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1EncryptionSpec
   */
  public function getEncryptionSpec()
  {
    return $this->encryptionSpec;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureOnlineStoreOptimized
   */
  public function setOptimized(GoogleCloudAiplatformV1FeatureOnlineStoreOptimized $optimized)
  {
    $this->optimized = $optimized;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureOnlineStoreOptimized
   */
  public function getOptimized()
  {
    return $this->optimized;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureOnlineStore::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureOnlineStore');
