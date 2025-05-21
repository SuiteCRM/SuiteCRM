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

class EnterpriseTopazSidekickMeetingNotesCardProto extends \Google\Model
{
  protected $eventType = EnterpriseTopazSidekickAgendaEntry::class;
  protected $eventDataType = '';
  /**
   * @var string
   */
  public $fileId;
  /**
   * @var string
   */
  public $title;
  /**
   * @var string
   */
  public $url;

  /**
   * @param EnterpriseTopazSidekickAgendaEntry
   */
  public function setEvent(EnterpriseTopazSidekickAgendaEntry $event)
  {
    $this->event = $event;
  }
  /**
   * @return EnterpriseTopazSidekickAgendaEntry
   */
  public function getEvent()
  {
    return $this->event;
  }
  /**
   * @param string
   */
  public function setFileId($fileId)
  {
    $this->fileId = $fileId;
  }
  /**
   * @return string
   */
  public function getFileId()
  {
    return $this->fileId;
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
class_alias(EnterpriseTopazSidekickMeetingNotesCardProto::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickMeetingNotesCardProto');
