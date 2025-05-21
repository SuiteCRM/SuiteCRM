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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementV1PrintJob extends \Google\Model
{
  /**
   * @var string
   */
  public $colorMode;
  /**
   * @var string
   */
  public $completeTime;
  /**
   * @var int
   */
  public $copyCount;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var int
   */
  public $documentPageCount;
  /**
   * @var string
   */
  public $duplexMode;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $printer;
  /**
   * @var string
   */
  public $printerId;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $title;
  /**
   * @var string
   */
  public $userEmail;
  /**
   * @var string
   */
  public $userId;

  /**
   * @param string
   */
  public function setColorMode($colorMode)
  {
    $this->colorMode = $colorMode;
  }
  /**
   * @return string
   */
  public function getColorMode()
  {
    return $this->colorMode;
  }
  /**
   * @param string
   */
  public function setCompleteTime($completeTime)
  {
    $this->completeTime = $completeTime;
  }
  /**
   * @return string
   */
  public function getCompleteTime()
  {
    return $this->completeTime;
  }
  /**
   * @param int
   */
  public function setCopyCount($copyCount)
  {
    $this->copyCount = $copyCount;
  }
  /**
   * @return int
   */
  public function getCopyCount()
  {
    return $this->copyCount;
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
   * @param int
   */
  public function setDocumentPageCount($documentPageCount)
  {
    $this->documentPageCount = $documentPageCount;
  }
  /**
   * @return int
   */
  public function getDocumentPageCount()
  {
    return $this->documentPageCount;
  }
  /**
   * @param string
   */
  public function setDuplexMode($duplexMode)
  {
    $this->duplexMode = $duplexMode;
  }
  /**
   * @return string
   */
  public function getDuplexMode()
  {
    return $this->duplexMode;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setPrinter($printer)
  {
    $this->printer = $printer;
  }
  /**
   * @return string
   */
  public function getPrinter()
  {
    return $this->printer;
  }
  /**
   * @param string
   */
  public function setPrinterId($printerId)
  {
    $this->printerId = $printerId;
  }
  /**
   * @return string
   */
  public function getPrinterId()
  {
    return $this->printerId;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
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
  public function setUserEmail($userEmail)
  {
    $this->userEmail = $userEmail;
  }
  /**
   * @return string
   */
  public function getUserEmail()
  {
    return $this->userEmail;
  }
  /**
   * @param string
   */
  public function setUserId($userId)
  {
    $this->userId = $userId;
  }
  /**
   * @return string
   */
  public function getUserId()
  {
    return $this->userId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementV1PrintJob::class, 'Google_Service_ChromeManagement_GoogleChromeManagementV1PrintJob');
