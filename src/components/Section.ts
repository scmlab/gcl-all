

export default function renderSection(title: string, sectionBody: string, subtitle?: string, code?: string) {
    return /* html */` 
      <div>
        <style scoped>
          .section {
            background-color: #f3f3f3;
            padding: 15px 25px;
          }
          .section-header {
            margin: 10px 0px;
          }
          .header-title {
            color: #616161;
            font-variant: small-caps;
            font-weight: bolder;
            font-size: 20px;
          }
          .header-subtitle {
            color: #959595;
            font-size: 16px;
          }
          .header-code {
            color: #959595;
            font-weight: bold;
            float: right;
            margin-top: 4px;
          }
          .section-body {
            background-color: white;
            padding: 10px 22px;
            font-family: monospace;
          }
        </style>

        <div class="section">
          <div class="section-header">
            <span>
              <span class="header-title">${title}</span>
              ${subtitle && /*html*/`
                <span class="header-subtitle">at ${subtitle}</span>`
              }
              <span class="header-code">#${code}</span>
            </span>
          </div>
          ${sectionBody && /*html*/`
            <div class="section-body">${sectionBody}</div>
          `}
        </div>
      </div>
    `
}