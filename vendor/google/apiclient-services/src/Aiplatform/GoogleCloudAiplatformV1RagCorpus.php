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

class GoogleCloudAiplatformV1RagCorpus extends \Google\Model
{
  protected $corpusStatusType = GoogleCloudAiplatformV1CorpusStatus::class;
  protected $corpusStatusDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $updateTime;
  protected $vectorDbConfigType = GoogleCloudAiplatformV1RagVectorDbConfig::class;
  protected $vectorDbConfigDataType = '';

  /**
   * @param GoogleCloudAiplatformV1CorpusStatus
   */
  public function setCorpusStatus(GoogleCloudAiplatformV1CorpusStatus $corpusStatus)
  {
    $this->corpusStatus = $corpusStatus;
  }
  /**
   * @return GoogleCloudAiplatformV1CorpusStatus
   */
  public function getCorpusStatus()
  {
    return $this->corpusStatus;
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
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
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
   * @param GoogleCloudAiplatformV1RagVectorDbConfig
   */
  public function setVectorDbConfig(GoogleCloudAiplatformV1RagVectorDbConfig $vectorDbConfig)
  {
    $this->vectorDbConfig = $vectorDbConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1RagVectorDbConfig
   */
  public function getVectorDbConfig()
  {
    return $this->vectorDbConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1RagCorpus::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1RagCorpus');
