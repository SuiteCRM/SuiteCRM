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

class GoogleCloudAiplatformV1CountTokensRequest extends \Google\Collection
{
  protected $collection_key = 'tools';
  protected $contentsType = GoogleCloudAiplatformV1Content::class;
  protected $contentsDataType = 'array';
  protected $generationConfigType = GoogleCloudAiplatformV1GenerationConfig::class;
  protected $generationConfigDataType = '';
  /**
   * @var array[]
   */
  public $instances;
  /**
   * @var string
   */
  public $model;
  protected $systemInstructionType = GoogleCloudAiplatformV1Content::class;
  protected $systemInstructionDataType = '';
  protected $toolsType = GoogleCloudAiplatformV1Tool::class;
  protected $toolsDataType = 'array';

  /**
   * @param GoogleCloudAiplatformV1Content[]
   */
  public function setContents($contents)
  {
    $this->contents = $contents;
  }
  /**
   * @return GoogleCloudAiplatformV1Content[]
   */
  public function getContents()
  {
    return $this->contents;
  }
  /**
   * @param GoogleCloudAiplatformV1GenerationConfig
   */
  public function setGenerationConfig(GoogleCloudAiplatformV1GenerationConfig $generationConfig)
  {
    $this->generationConfig = $generationConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1GenerationConfig
   */
  public function getGenerationConfig()
  {
    return $this->generationConfig;
  }
  /**
   * @param array[]
   */
  public function setInstances($instances)
  {
    $this->instances = $instances;
  }
  /**
   * @return array[]
   */
  public function getInstances()
  {
    return $this->instances;
  }
  /**
   * @param string
   */
  public function setModel($model)
  {
    $this->model = $model;
  }
  /**
   * @return string
   */
  public function getModel()
  {
    return $this->model;
  }
  /**
   * @param GoogleCloudAiplatformV1Content
   */
  public function setSystemInstruction(GoogleCloudAiplatformV1Content $systemInstruction)
  {
    $this->systemInstruction = $systemInstruction;
  }
  /**
   * @return GoogleCloudAiplatformV1Content
   */
  public function getSystemInstruction()
  {
    return $this->systemInstruction;
  }
  /**
   * @param GoogleCloudAiplatformV1Tool[]
   */
  public function setTools($tools)
  {
    $this->tools = $tools;
  }
  /**
   * @return GoogleCloudAiplatformV1Tool[]
   */
  public function getTools()
  {
    return $this->tools;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1CountTokensRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1CountTokensRequest');
