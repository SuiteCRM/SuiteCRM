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

namespace Google\Service\CloudNaturalLanguage;

class XPSImageModelArtifactSpec extends \Google\Collection
{
  protected $collection_key = 'exportArtifact';
  protected $checkpointArtifactType = XPSModelArtifactItem::class;
  protected $checkpointArtifactDataType = '';
  protected $exportArtifactType = XPSModelArtifactItem::class;
  protected $exportArtifactDataType = 'array';
  /**
   * @var string
   */
  public $labelGcsUri;
  protected $servingArtifactType = XPSModelArtifactItem::class;
  protected $servingArtifactDataType = '';
  /**
   * @var string
   */
  public $tfJsBinaryGcsPrefix;
  /**
   * @var string
   */
  public $tfLiteMetadataGcsUri;

  /**
   * @param XPSModelArtifactItem
   */
  public function setCheckpointArtifact(XPSModelArtifactItem $checkpointArtifact)
  {
    $this->checkpointArtifact = $checkpointArtifact;
  }
  /**
   * @return XPSModelArtifactItem
   */
  public function getCheckpointArtifact()
  {
    return $this->checkpointArtifact;
  }
  /**
   * @param XPSModelArtifactItem[]
   */
  public function setExportArtifact($exportArtifact)
  {
    $this->exportArtifact = $exportArtifact;
  }
  /**
   * @return XPSModelArtifactItem[]
   */
  public function getExportArtifact()
  {
    return $this->exportArtifact;
  }
  /**
   * @param string
   */
  public function setLabelGcsUri($labelGcsUri)
  {
    $this->labelGcsUri = $labelGcsUri;
  }
  /**
   * @return string
   */
  public function getLabelGcsUri()
  {
    return $this->labelGcsUri;
  }
  /**
   * @param XPSModelArtifactItem
   */
  public function setServingArtifact(XPSModelArtifactItem $servingArtifact)
  {
    $this->servingArtifact = $servingArtifact;
  }
  /**
   * @return XPSModelArtifactItem
   */
  public function getServingArtifact()
  {
    return $this->servingArtifact;
  }
  /**
   * @param string
   */
  public function setTfJsBinaryGcsPrefix($tfJsBinaryGcsPrefix)
  {
    $this->tfJsBinaryGcsPrefix = $tfJsBinaryGcsPrefix;
  }
  /**
   * @return string
   */
  public function getTfJsBinaryGcsPrefix()
  {
    return $this->tfJsBinaryGcsPrefix;
  }
  /**
   * @param string
   */
  public function setTfLiteMetadataGcsUri($tfLiteMetadataGcsUri)
  {
    $this->tfLiteMetadataGcsUri = $tfLiteMetadataGcsUri;
  }
  /**
   * @return string
   */
  public function getTfLiteMetadataGcsUri()
  {
    return $this->tfLiteMetadataGcsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSImageModelArtifactSpec::class, 'Google_Service_CloudNaturalLanguage_XPSImageModelArtifactSpec');
