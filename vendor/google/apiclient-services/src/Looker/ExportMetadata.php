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

namespace Google\Service\Looker;

class ExportMetadata extends \Google\Collection
{
  protected $collection_key = 'filePaths';
  protected $exportEncryptionKeyType = ExportMetadataEncryptionKey::class;
  protected $exportEncryptionKeyDataType = '';
  /**
   * @var string[]
   */
  public $filePaths;
  /**
   * @var string
   */
  public $lookerEncryptionKey;
  /**
   * @var string
   */
  public $lookerInstance;
  /**
   * @var string
   */
  public $lookerPlatformEdition;
  /**
   * @var string
   */
  public $lookerVersion;
  /**
   * @var string
   */
  public $source;

  /**
   * @param ExportMetadataEncryptionKey
   */
  public function setExportEncryptionKey(ExportMetadataEncryptionKey $exportEncryptionKey)
  {
    $this->exportEncryptionKey = $exportEncryptionKey;
  }
  /**
   * @return ExportMetadataEncryptionKey
   */
  public function getExportEncryptionKey()
  {
    return $this->exportEncryptionKey;
  }
  /**
   * @param string[]
   */
  public function setFilePaths($filePaths)
  {
    $this->filePaths = $filePaths;
  }
  /**
   * @return string[]
   */
  public function getFilePaths()
  {
    return $this->filePaths;
  }
  /**
   * @param string
   */
  public function setLookerEncryptionKey($lookerEncryptionKey)
  {
    $this->lookerEncryptionKey = $lookerEncryptionKey;
  }
  /**
   * @return string
   */
  public function getLookerEncryptionKey()
  {
    return $this->lookerEncryptionKey;
  }
  /**
   * @param string
   */
  public function setLookerInstance($lookerInstance)
  {
    $this->lookerInstance = $lookerInstance;
  }
  /**
   * @return string
   */
  public function getLookerInstance()
  {
    return $this->lookerInstance;
  }
  /**
   * @param string
   */
  public function setLookerPlatformEdition($lookerPlatformEdition)
  {
    $this->lookerPlatformEdition = $lookerPlatformEdition;
  }
  /**
   * @return string
   */
  public function getLookerPlatformEdition()
  {
    return $this->lookerPlatformEdition;
  }
  /**
   * @param string
   */
  public function setLookerVersion($lookerVersion)
  {
    $this->lookerVersion = $lookerVersion;
  }
  /**
   * @return string
   */
  public function getLookerVersion()
  {
    return $this->lookerVersion;
  }
  /**
   * @param string
   */
  public function setSource($source)
  {
    $this->source = $source;
  }
  /**
   * @return string
   */
  public function getSource()
  {
    return $this->source;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExportMetadata::class, 'Google_Service_Looker_ExportMetadata');
