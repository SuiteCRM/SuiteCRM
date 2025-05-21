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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickCommonDocument extends \Google\Model
{
  /**
   * @var string
   */
  public $accessType;
  protected $debugInfoType = EnterpriseTopazSidekickCommonDebugInfo::class;
  protected $debugInfoDataType = '';
  /**
   * @var string
   */
  public $documentId;
  protected $driveDocumentMetadataType = EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata::class;
  protected $driveDocumentMetadataDataType = '';
  /**
   * @var string
   */
  public $genericUrl;
  protected $justificationType = EnterpriseTopazSidekickCommonDocumentJustification::class;
  protected $justificationDataType = '';
  /**
   * @var string
   */
  public $mimeType;
  /**
   * @var string
   */
  public $provenance;
  /**
   * @var string
   */
  public $reason;
  /**
   * @var string
   */
  public $snippet;
  /**
   * @var string
   */
  public $thumbnailUrl;
  /**
   * @var string
   */
  public $title;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $url;

  /**
   * @param string
   */
  public function setAccessType($accessType)
  {
    $this->accessType = $accessType;
  }
  /**
   * @return string
   */
  public function getAccessType()
  {
    return $this->accessType;
  }
  /**
   * @param EnterpriseTopazSidekickCommonDebugInfo
   */
  public function setDebugInfo(EnterpriseTopazSidekickCommonDebugInfo $debugInfo)
  {
    $this->debugInfo = $debugInfo;
  }
  /**
   * @return EnterpriseTopazSidekickCommonDebugInfo
   */
  public function getDebugInfo()
  {
    return $this->debugInfo;
  }
  /**
   * @param string
   */
  public function setDocumentId($documentId)
  {
    $this->documentId = $documentId;
  }
  /**
   * @return string
   */
  public function getDocumentId()
  {
    return $this->documentId;
  }
  /**
   * @param EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata
   */
  public function setDriveDocumentMetadata(EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata $driveDocumentMetadata)
  {
    $this->driveDocumentMetadata = $driveDocumentMetadata;
  }
  /**
   * @return EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata
   */
  public function getDriveDocumentMetadata()
  {
    return $this->driveDocumentMetadata;
  }
  /**
   * @param string
   */
  public function setGenericUrl($genericUrl)
  {
    $this->genericUrl = $genericUrl;
  }
  /**
   * @return string
   */
  public function getGenericUrl()
  {
    return $this->genericUrl;
  }
  /**
   * @param EnterpriseTopazSidekickCommonDocumentJustification
   */
  public function setJustification(EnterpriseTopazSidekickCommonDocumentJustification $justification)
  {
    $this->justification = $justification;
  }
  /**
   * @return EnterpriseTopazSidekickCommonDocumentJustification
   */
  public function getJustification()
  {
    return $this->justification;
  }
  /**
   * @param string
   */
  public function setMimeType($mimeType)
  {
    $this->mimeType = $mimeType;
  }
  /**
   * @return string
   */
  public function getMimeType()
  {
    return $this->mimeType;
  }
  /**
   * @param string
   */
  public function setProvenance($provenance)
  {
    $this->provenance = $provenance;
  }
  /**
   * @return string
   */
  public function getProvenance()
  {
    return $this->provenance;
  }
  /**
   * @param string
   */
  public function setReason($reason)
  {
    $this->reason = $reason;
  }
  /**
   * @return string
   */
  public function getReason()
  {
    return $this->reason;
  }
  /**
   * @param string
   */
  public function setSnippet($snippet)
  {
    $this->snippet = $snippet;
  }
  /**
   * @return string
   */
  public function getSnippet()
  {
    return $this->snippet;
  }
  /**
   * @param string
   */
  public function setThumbnailUrl($thumbnailUrl)
  {
    $this->thumbnailUrl = $thumbnailUrl;
  }
  /**
   * @return string
   */
  public function getThumbnailUrl()
  {
    return $this->thumbnailUrl;
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
  public function setUrl($url)
  {
    $this->url = $url;
  }
  /**
   * @return string
   */
  public function getUrl()
  {
    return $this->url;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickCommonDocument::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickCommonDocument');
