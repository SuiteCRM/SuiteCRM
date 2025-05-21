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

class GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfigSourceEntry extends \Google\Model
{
  /**
   * @var string
   */
  public $enterpriseDatastore;
  /**
   * @var string
   */
  public $inlineContext;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $vertexAiSearchDatastore;

  /**
   * @param string
   */
  public function setEnterpriseDatastore($enterpriseDatastore)
  {
    $this->enterpriseDatastore = $enterpriseDatastore;
  }
  /**
   * @return string
   */
  public function getEnterpriseDatastore()
  {
    return $this->enterpriseDatastore;
  }
  /**
   * @param string
   */
  public function setInlineContext($inlineContext)
  {
    $this->inlineContext = $inlineContext;
  }
  /**
   * @return string
   */
  public function getInlineContext()
  {
    return $this->inlineContext;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setVertexAiSearchDatastore($vertexAiSearchDatastore)
  {
    $this->vertexAiSearchDatastore = $vertexAiSearchDatastore;
  }
  /**
   * @return string
   */
  public function getVertexAiSearchDatastore()
  {
    return $this->vertexAiSearchDatastore;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfigSourceEntry::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfigSourceEntry');
