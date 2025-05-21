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

class XPSFileSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $directoryPath;
  /**
   * @var string
   */
  public $fileFormat;
  /**
   * @var string
   */
  public $fileSpec;
  /**
   * @var string
   */
  public $singleFilePath;

  /**
   * @param string
   */
  public function setDirectoryPath($directoryPath)
  {
    $this->directoryPath = $directoryPath;
  }
  /**
   * @return string
   */
  public function getDirectoryPath()
  {
    return $this->directoryPath;
  }
  /**
   * @param string
   */
  public function setFileFormat($fileFormat)
  {
    $this->fileFormat = $fileFormat;
  }
  /**
   * @return string
   */
  public function getFileFormat()
  {
    return $this->fileFormat;
  }
  /**
   * @param string
   */
  public function setFileSpec($fileSpec)
  {
    $this->fileSpec = $fileSpec;
  }
  /**
   * @return string
   */
  public function getFileSpec()
  {
    return $this->fileSpec;
  }
  /**
   * @param string
   */
  public function setSingleFilePath($singleFilePath)
  {
    $this->singleFilePath = $singleFilePath;
  }
  /**
   * @return string
   */
  public function getSingleFilePath()
  {
    return $this->singleFilePath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSFileSpec::class, 'Google_Service_CloudNaturalLanguage_XPSFileSpec');
