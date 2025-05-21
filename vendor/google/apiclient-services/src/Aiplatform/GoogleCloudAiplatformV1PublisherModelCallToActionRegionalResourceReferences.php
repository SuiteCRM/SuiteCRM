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

class GoogleCloudAiplatformV1PublisherModelCallToActionRegionalResourceReferences extends \Google\Model
{
  protected $referencesType = GoogleCloudAiplatformV1PublisherModelResourceReference::class;
  protected $referencesDataType = 'map';
  /**
   * @var string
   */
  public $resourceDescription;
  /**
   * @var string
   */
  public $resourceTitle;
  /**
   * @var string
   */
  public $resourceUseCase;
  /**
   * @var string
   */
  public $title;

  /**
   * @param GoogleCloudAiplatformV1PublisherModelResourceReference[]
   */
  public function setReferences($references)
  {
    $this->references = $references;
  }
  /**
   * @return GoogleCloudAiplatformV1PublisherModelResourceReference[]
   */
  public function getReferences()
  {
    return $this->references;
  }
  /**
   * @param string
   */
  public function setResourceDescription($resourceDescription)
  {
    $this->resourceDescription = $resourceDescription;
  }
  /**
   * @return string
   */
  public function getResourceDescription()
  {
    return $this->resourceDescription;
  }
  /**
   * @param string
   */
  public function setResourceTitle($resourceTitle)
  {
    $this->resourceTitle = $resourceTitle;
  }
  /**
   * @return string
   */
  public function getResourceTitle()
  {
    return $this->resourceTitle;
  }
  /**
   * @param string
   */
  public function setResourceUseCase($resourceUseCase)
  {
    $this->resourceUseCase = $resourceUseCase;
  }
  /**
   * @return string
   */
  public function getResourceUseCase()
  {
    return $this->resourceUseCase;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PublisherModelCallToActionRegionalResourceReferences::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PublisherModelCallToActionRegionalResourceReferences');
