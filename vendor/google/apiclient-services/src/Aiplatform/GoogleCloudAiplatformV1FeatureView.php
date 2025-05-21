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

class GoogleCloudAiplatformV1FeatureView extends \Google\Model
{
  protected $bigQuerySourceType = GoogleCloudAiplatformV1FeatureViewBigQuerySource::class;
  protected $bigQuerySourceDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $etag;
  protected $featureRegistrySourceType = GoogleCloudAiplatformV1FeatureViewFeatureRegistrySource::class;
  protected $featureRegistrySourceDataType = '';
  protected $indexConfigType = GoogleCloudAiplatformV1FeatureViewIndexConfig::class;
  protected $indexConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $optimizedConfigType = GoogleCloudAiplatformV1FeatureViewOptimizedConfig::class;
  protected $optimizedConfigDataType = '';
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
  public $serviceAccountEmail;
  /**
   * @var string
   */
  public $serviceAgentType;
  protected $syncConfigType = GoogleCloudAiplatformV1FeatureViewSyncConfig::class;
  protected $syncConfigDataType = '';
  /**
   * @var string
   */
  public $updateTime;
  protected $vertexRagSourceType = GoogleCloudAiplatformV1FeatureViewVertexRagSource::class;
  protected $vertexRagSourceDataType = '';

  /**
   * @param GoogleCloudAiplatformV1FeatureViewBigQuerySource
   */
  public function setBigQuerySource(GoogleCloudAiplatformV1FeatureViewBigQuerySource $bigQuerySource)
  {
    $this->bigQuerySource = $bigQuerySource;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureViewBigQuerySource
   */
  public function getBigQuerySource()
  {
    return $this->bigQuerySource;
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
   * @param GoogleCloudAiplatformV1FeatureViewFeatureRegistrySource
   */
  public function setFeatureRegistrySource(GoogleCloudAiplatformV1FeatureViewFeatureRegistrySource $featureRegistrySource)
  {
    $this->featureRegistrySource = $featureRegistrySource;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureViewFeatureRegistrySource
   */
  public function getFeatureRegistrySource()
  {
    return $this->featureRegistrySource;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureViewIndexConfig
   */
  public function setIndexConfig(GoogleCloudAiplatformV1FeatureViewIndexConfig $indexConfig)
  {
    $this->indexConfig = $indexConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureViewIndexConfig
   */
  public function getIndexConfig()
  {
    return $this->indexConfig;
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
   * @param GoogleCloudAiplatformV1FeatureViewOptimizedConfig
   */
  public function setOptimizedConfig(GoogleCloudAiplatformV1FeatureViewOptimizedConfig $optimizedConfig)
  {
    $this->optimizedConfig = $optimizedConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureViewOptimizedConfig
   */
  public function getOptimizedConfig()
  {
    return $this->optimizedConfig;
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
  public function setServiceAccountEmail($serviceAccountEmail)
  {
    $this->serviceAccountEmail = $serviceAccountEmail;
  }
  /**
   * @return string
   */
  public function getServiceAccountEmail()
  {
    return $this->serviceAccountEmail;
  }
  /**
   * @param string
   */
  public function setServiceAgentType($serviceAgentType)
  {
    $this->serviceAgentType = $serviceAgentType;
  }
  /**
   * @return string
   */
  public function getServiceAgentType()
  {
    return $this->serviceAgentType;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureViewSyncConfig
   */
  public function setSyncConfig(GoogleCloudAiplatformV1FeatureViewSyncConfig $syncConfig)
  {
    $this->syncConfig = $syncConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureViewSyncConfig
   */
  public function getSyncConfig()
  {
    return $this->syncConfig;
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
  /**
   * @param GoogleCloudAiplatformV1FeatureViewVertexRagSource
   */
  public function setVertexRagSource(GoogleCloudAiplatformV1FeatureViewVertexRagSource $vertexRagSource)
  {
    $this->vertexRagSource = $vertexRagSource;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureViewVertexRagSource
   */
  public function getVertexRagSource()
  {
    return $this->vertexRagSource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureView::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureView');
