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

namespace Google\Service\DataCatalog;

class GoogleCloudDatacatalogV1VertexModelSpec extends \Google\Collection
{
  protected $collection_key = 'versionAliases';
  /**
   * @var string
   */
  public $containerImageUri;
  /**
   * @var string[]
   */
  public $versionAliases;
  /**
   * @var string
   */
  public $versionDescription;
  /**
   * @var string
   */
  public $versionId;
  protected $vertexModelSourceInfoType = GoogleCloudDatacatalogV1VertexModelSourceInfo::class;
  protected $vertexModelSourceInfoDataType = '';

  /**
   * @param string
   */
  public function setContainerImageUri($containerImageUri)
  {
    $this->containerImageUri = $containerImageUri;
  }
  /**
   * @return string
   */
  public function getContainerImageUri()
  {
    return $this->containerImageUri;
  }
  /**
   * @param string[]
   */
  public function setVersionAliases($versionAliases)
  {
    $this->versionAliases = $versionAliases;
  }
  /**
   * @return string[]
   */
  public function getVersionAliases()
  {
    return $this->versionAliases;
  }
  /**
   * @param string
   */
  public function setVersionDescription($versionDescription)
  {
    $this->versionDescription = $versionDescription;
  }
  /**
   * @return string
   */
  public function getVersionDescription()
  {
    return $this->versionDescription;
  }
  /**
   * @param string
   */
  public function setVersionId($versionId)
  {
    $this->versionId = $versionId;
  }
  /**
   * @return string
   */
  public function getVersionId()
  {
    return $this->versionId;
  }
  /**
   * @param GoogleCloudDatacatalogV1VertexModelSourceInfo
   */
  public function setVertexModelSourceInfo(GoogleCloudDatacatalogV1VertexModelSourceInfo $vertexModelSourceInfo)
  {
    $this->vertexModelSourceInfo = $vertexModelSourceInfo;
  }
  /**
   * @return GoogleCloudDatacatalogV1VertexModelSourceInfo
   */
  public function getVertexModelSourceInfo()
  {
    return $this->vertexModelSourceInfo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDatacatalogV1VertexModelSpec::class, 'Google_Service_DataCatalog_GoogleCloudDatacatalogV1VertexModelSpec');
