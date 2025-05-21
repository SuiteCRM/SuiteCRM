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

class GoogleCloudAiplatformV1PublisherModelCallToActionDeploy extends \Google\Model
{
  /**
   * @var string
   */
  public $artifactUri;
  protected $automaticResourcesType = GoogleCloudAiplatformV1AutomaticResources::class;
  protected $automaticResourcesDataType = '';
  protected $containerSpecType = GoogleCloudAiplatformV1ModelContainerSpec::class;
  protected $containerSpecDataType = '';
  protected $dedicatedResourcesType = GoogleCloudAiplatformV1DedicatedResources::class;
  protected $dedicatedResourcesDataType = '';
  protected $deployMetadataType = GoogleCloudAiplatformV1PublisherModelCallToActionDeployDeployMetadata::class;
  protected $deployMetadataDataType = '';
  /**
   * @var string
   */
  public $deployTaskName;
  protected $largeModelReferenceType = GoogleCloudAiplatformV1LargeModelReference::class;
  protected $largeModelReferenceDataType = '';
  /**
   * @var string
   */
  public $modelDisplayName;
  /**
   * @var string
   */
  public $publicArtifactUri;
  /**
   * @var string
   */
  public $sharedResources;
  /**
   * @var string
   */
  public $title;

  /**
   * @param string
   */
  public function setArtifactUri($artifactUri)
  {
    $this->artifactUri = $artifactUri;
  }
  /**
   * @return string
   */
  public function getArtifactUri()
  {
    return $this->artifactUri;
  }
  /**
   * @param GoogleCloudAiplatformV1AutomaticResources
   */
  public function setAutomaticResources(GoogleCloudAiplatformV1AutomaticResources $automaticResources)
  {
    $this->automaticResources = $automaticResources;
  }
  /**
   * @return GoogleCloudAiplatformV1AutomaticResources
   */
  public function getAutomaticResources()
  {
    return $this->automaticResources;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelContainerSpec
   */
  public function setContainerSpec(GoogleCloudAiplatformV1ModelContainerSpec $containerSpec)
  {
    $this->containerSpec = $containerSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelContainerSpec
   */
  public function getContainerSpec()
  {
    return $this->containerSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1DedicatedResources
   */
  public function setDedicatedResources(GoogleCloudAiplatformV1DedicatedResources $dedicatedResources)
  {
    $this->dedicatedResources = $dedicatedResources;
  }
  /**
   * @return GoogleCloudAiplatformV1DedicatedResources
   */
  public function getDedicatedResources()
  {
    return $this->dedicatedResources;
  }
  /**
   * @param GoogleCloudAiplatformV1PublisherModelCallToActionDeployDeployMetadata
   */
  public function setDeployMetadata(GoogleCloudAiplatformV1PublisherModelCallToActionDeployDeployMetadata $deployMetadata)
  {
    $this->deployMetadata = $deployMetadata;
  }
  /**
   * @return GoogleCloudAiplatformV1PublisherModelCallToActionDeployDeployMetadata
   */
  public function getDeployMetadata()
  {
    return $this->deployMetadata;
  }
  /**
   * @param string
   */
  public function setDeployTaskName($deployTaskName)
  {
    $this->deployTaskName = $deployTaskName;
  }
  /**
   * @return string
   */
  public function getDeployTaskName()
  {
    return $this->deployTaskName;
  }
  /**
   * @param GoogleCloudAiplatformV1LargeModelReference
   */
  public function setLargeModelReference(GoogleCloudAiplatformV1LargeModelReference $largeModelReference)
  {
    $this->largeModelReference = $largeModelReference;
  }
  /**
   * @return GoogleCloudAiplatformV1LargeModelReference
   */
  public function getLargeModelReference()
  {
    return $this->largeModelReference;
  }
  /**
   * @param string
   */
  public function setModelDisplayName($modelDisplayName)
  {
    $this->modelDisplayName = $modelDisplayName;
  }
  /**
   * @return string
   */
  public function getModelDisplayName()
  {
    return $this->modelDisplayName;
  }
  /**
   * @param string
   */
  public function setPublicArtifactUri($publicArtifactUri)
  {
    $this->publicArtifactUri = $publicArtifactUri;
  }
  /**
   * @return string
   */
  public function getPublicArtifactUri()
  {
    return $this->publicArtifactUri;
  }
  /**
   * @param string
   */
  public function setSharedResources($sharedResources)
  {
    $this->sharedResources = $sharedResources;
  }
  /**
   * @return string
   */
  public function getSharedResources()
  {
    return $this->sharedResources;
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
class_alias(GoogleCloudAiplatformV1PublisherModelCallToActionDeploy::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PublisherModelCallToActionDeploy');
